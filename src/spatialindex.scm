;------------------------------------------------------------------------------;
;                                                                              ;
;  MiniLight Scheme : minimal global illumination renderer                     ;
;  Harrison Ainsworth / HXA7241 : 2010, 2011, 2013                             ;
;                                                                              ;
;  http://www.hxa.name/minilight                                               ;
;                                                                              ;
;------------------------------------------------------------------------------;


;; uses:
;; * general
;; * objects
;; * vector3r
;; * triangle




;; Minimal spatial index for ray tracing.
;;
;; Suitable for a scale of 1 numerical unit == 1 metre, and with a resolution
;; of 1 millimetre. (Implementation uses fixed tolerances.)
;;
;; Constant.
;;
;; @implementation
;; A crude State pattern: typed by branch? field to be either a branch
;; or leaf cell.
;;
;; Octree: axis-aligned, cubical. Subcells are numbered thusly:
;;            110---111
;;            /|    /|
;;         010---011 |
;;    y z   | 100-|-101
;;    |/    |/    | /
;;    .-x  000---001
;;
;; Each cell stores its bound (fatter data, but simpler code).
;;
;; Calculations for building and tracing are absolute rather than incremental --
;; so quite numerically solid. Uses tolerances in: bounding triangles (in
;; TriangleBound), and checking intersection is inside cell (both effective
;; for axis-aligned items). Also, depth is constrained to an absolute subcell
;; size (easy way to handle overlapping items).
;;
;; @invariants
;; * branch? is a boolean
;; * bound is a pair of Vector3r
;;   * (car bound) <= (cdr bound)
;;   * bound encompasses the cell's contents
;; if branch?
;;   * subParts is a vector, length == 8
;;   * subParts elements are SpatialIndex or #f
;; else
;;   * subParts is a vector
;;   * subParts elements are Triangle




;; initialisation ----------------------------------------------------------- ;;

;; Construct a SpatialIndex.
;;
;; @param  eyePosition [Vector3r]         position of eye point
;; @param  items       [vector[Triangle]] items to be indexed
;; @return [SpatialIndex]
;;
(define (SpatialIndex.create eyePosition items)

  ;; make overall bound
  (let ((bound
          (let* (
                 ;; make rectilinear bound
                 (rectBound
                   (foldv items
                          ;; include eye position
                          ;; (makes intersection algorithm simpler)
                          (cons eyePosition eyePosition)
                          (lambda (rb item)
                            (let ((itemBound (item 'bound)))
                              ;; expand to fit item
                              (cons (^clampedMax (car itemBound) (car rb))
                                    (^clampedMin (cdr itemBound) (cdr rb)))))))

                 ;; make cubical upper bound
                 (cubeUpper
                   ;; find max dimension
                   (let ((maxSize (^apply max (^- (cdr rectBound)
                                                  (car rectBound)))))
                     ;; set all dimensions to max
                     (^clampedMin (^+ (car rectBound) (^* ^ONE maxSize))
                                  (cdr rectBound)))))

            ;; make cubical bound, by replacing rect upper
            (cons (car rectBound) cubeUpper))))

    ;; make subcell tree
    ;; -- delegate to main (recursive) constructor
    (SpatialIndex.construct_ bound items 0)))


;; Main recursive constructor.
;;
;; @param  bound [pair[Vector3r Vector3r]] lower and upper corners
;; @param  items [vector[Triangle]]        items remaining to insert
;; @param  level [integer]                 depth in the tree
;; @return [SpatialIndex]
;;
(define (SpatialIndex.construct_ bound items level)

  ;; check if items overflow leaf and tree not too deep
  (if (and (> (vector-length items) MAX_ITEMS_)
           (< level (- MAX_LEVELS_ 1)))

    ;; make branch: make subcells, and recurse construction
    (let* ((!q          0)
           (makeSubcell
             (lambda (subcellIndex _)
               (let* (
                      ;; make subcell bound
                      (subBound
                        (let ((middle (^* (^+ (car bound) (cdr bound)) 0.5))
                              (choose (lambda (v0 v1)
                                        (^unfold (lambda (i _)
                                                   (if (bit? subcellIndex i)
                                                     (^i v1 i)
                                                     (^i v0 i)))))))
                          ;; choose axes of: lower or middle, and
                          ;; middle or upper; according to index bits
                          (cons (choose (car bound) middle)
                                (choose middle (cdr bound)))))

                      ;; collect items that overlap subcell
                      (subItems
                        (filterv items
                                 (lambda (item)
                                   (let ((itemBound (item 'bound))
                                         ;; overlap test for half bound
                                         (ov (lambda (b0 b1)
                                               (not (negative?
                                                 (^apply min
                                                         (^- (cdr b0)
                                                             (car b1))))))))
                                     (and (ov itemBound subBound)
                                          (ov subBound itemBound)))))))

                 (if (= (vector-length subItems) (vector-length items))
                   (set! !q (+ !q 1)))

                 ;; maybe make subcell, if any overlapping subitems
                 (if (not (zero? (vector-length subItems)))
                   ;; curtail degenerate subdivision by adjusting next level
                   ;; (degenerate if two or more subcells copy entire
                   ;; contents of parent, or if subdivision reaches below
                   ;; mm size) (having a model including the sun requires
                   ;; one subcell copying entire contents of parent to be
                   ;; allowed)
                   (let ((nextLevel (if (or (> !q 1)
                                            (< (^apply min (^- (cdr subBound)
                                                               (car subBound)))
                                               (* TOLERANCE 4.0)))
                                      MAX_LEVELS_
                                      (+ level 1))))
                     ;; recurse
                     (SpatialIndex.construct_ subBound subItems nextLevel))
                   #f)))))

      (defobject SpatialIndex
        (branch?  #t)
        (bound    bound)
        (subParts (unfoldv 8 #f makeSubcell))))

    ;; make leaf: store items, and end recursion
    (defobject SpatialIndex
      (branch?  #f)
      (bound    bound)
      (subParts items))))




(defclass SpatialIndex

;; queries ------------------------------------------------------------------ ;;

  ;; Find nearest intersection of ray with item.
  ;;
  ;; @param  rayOrigin    [Vector3r]    ray start point
  ;; @param  rayDirection [Vector3r]    ray direction (unitized)
  ;; @param  lastHit      [Triangle]    previous item intersected
  ;; @param  start_       [Vector3r|#f] (internal use) walk point, or #f
  ;; @return [pair[Triangle Vector3r]|boolean] hit object and position, or #f
  ;;
  (method (intersection self rayOrigin rayDirection lastHit start_)

    (if (self 'branch?)
      (self 'intersectBranch_ rayOrigin rayDirection lastHit start_)
      (self 'intersectLeaf_   rayOrigin rayDirection lastHit)))


  ;; Find nearest intersection of ray with branch.
  ;;
  ;; @param  rayOrigin    [Vector3r]    ray start point
  ;; @param  rayDirection [Vector3r]    ray direction (unitized)
  ;; @param  lastHit      [Triangle]    previous item intersected
  ;; @param  start_       [Vector3r|#f] walk point, or #f
  ;; @return [pair[Triangle Vector3r]|boolean] hit object and position, or #f
  ;;
  (method (intersectBranch_ self rayOrigin rayDirection lastHit start_)

    ;; step through subcells and recurse
    (let* ((bound    (self 'bound))
           (subParts (self 'subParts))

           ;; default walk start at ray origin
           (start (or start_ rayOrigin))

           ;; find which subcell holds walk point (walk point is inside cell)
           (subIndex (let* (
                            ;; compare dimension with center
                            (center (^* (^+ (car bound) (cdr bound)) 0.5))
                            (coord  (^bop (lambda (a b) (if (>= a b) 1.0 0.0))
                                          start center)))
                       ;; make subcell index from coord
                       (inexact->exact (^. coord (^create 1.0 2.0 4.0))))))

      ;; step through intersected subcells
      (let walk ((cellIndex    subIndex)
                 (cellPosition start))

        ;; intersect subcell, returning hit if successful
        (or (let ((subCell (vector-ref subParts cellIndex)))
              (and subCell
                   (subCell 'intersection rayOrigin rayDirection lastHit
                                          cellPosition)))

        ;; or if no hit, continue walking across subcells
            ;; find next subcell ray moves to
            ;; (by finding which face of corner ahead is crossed first)
            (let ((stepAxis
                    (do ((i        0 (+ i 1))
                         ;; find which face (inter-/outer-) the ray is heading
                         ;; for (in this dimension)
                         (stepAxis (cons #f 0)
                           (let* ((dir  (^i rayDirection i))
                                  (high (bit? cellIndex i))
                                  (face (^i (if (not (eq? (negative? dir) high))
                                              (if high (cdr bound) (car bound))
                                              (^* (^+ (car bound)
                                                      (cdr bound)) 0.5)) i))
                                  ;; calculate distance to face
                                  (distance
                                    ;; use false instead of div by zero
                                    (and (not (zero? dir))
                                         (/ (- face (^i rayOrigin i)) dir))))
                             ;; update if nearer
                             (if (or (not (car stepAxis))
                                     (and distance (< distance (car stepAxis))))
                               (cons distance i)
                               stepAxis))))
                        ((>= i 3) stepAxis))))

              (let ((indexSign (if (bit? cellIndex (cdr stepAxis)) -1 +1)))
                ;; leaving branch if: direction is negative and subcell is low,
                ;; or direction is positive and subcell is high
                (and (not (negative? (* (^i rayDirection (cdr stepAxis))
                                        indexSign)))
                     ;; move to (outer face of) next subcell
                     (walk (+ cellIndex (* indexSign (expt 2 (cdr stepAxis))))
                           (^+ rayOrigin (^* rayDirection (car stepAxis)))
                           ))))))))


  ;; Find nearest intersection of ray with leaf.
  ;;
  ;; @param  rayOrigin    [Vector3r] ray start point
  ;; @param  rayDirection [Vector3r] ray direction (unitized)
  ;; @param  lastHit      [Triangle] previous item intersected
  ;; @return [pair[Triangle Vector3r]|boolean] hit object and position, or #f
  ;;
  (method (intersectLeaf_ self rayOrigin rayDirection lastHit)

    (let ((bound (self 'bound))
          (items (self 'subParts)))

      ;; exhaustively intersect contained items
      (car
        (foldv items (cons #f #f)
               (lambda (nearest item)
                 ;; return new nearest, or previous nearest
                 (or (and
                          ;; avoid spurious intersection with surface just come
                          ;; from
                          (not (eq? item lastHit))

                          ;; intersect ray with item and inspect if nearest so
                          ;; far
                          (let ((distance (item 'intersection rayOrigin
                                                              rayDirection)))
                            (and distance (or (not (cdr nearest))
                                              (< distance (cdr nearest)))
                                 ;; check intersection is inside cell bound
                                 ;; (with tolerance)
                                 (let ((hit (^+ rayOrigin
                                                (^* rayDirection distance)))
                                       (t   TOLERANCE))
                                   (and (<= (^apply max (^- (car bound) hit)) t)
                                        (<= (^apply max (^- hit (cdr bound))) t)

                                        ;; new nearest so far
                                        (cons (cons item hit) distance))))))
                     nearest))))))

)




;; constants ---------------------------------------------------------------- ;;

;; Maximum levels of nesting.
;; Accommodates scene including sun and earth, down to cm cells
;; (use 47 for mm)
;;
(define MAX_LEVELS_ 44)


;; Maximum items per leaf (when below maximum nesting level).
;; 8 seemed reasonably optimal in casual testing
;;
(define MAX_ITEMS_ 8)
