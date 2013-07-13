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




;; Simple, explicit/non-vertex-shared, triangle.
;;
;; Includes geometry and quality.
;;
;; Constant.
;;
;; @implementation
;; Adapts ray intersection code from:
;; 'Fast, Minimum Storage Ray-Triangle Intersection';
;; Moller, Trumbore;
;; 1997.
;; Journal of Graphics Tools, v2 n1 p21.
;; http://www.acm.org/jgt/papers/MollerTrumbore97/
;;
;; @invariants
;; * vertexs is a vector of Vector3r, with length == 3
;; * reflectivity is a Vector3r >= 0 and <= 1
;; * emitivity    is a Vector3r >= 0




;; initialisation ----------------------------------------------------------- ;;

;; Construct a Triangle.
;;
;; @param  !fileIn [port] model input file
;; @return [Triangle]
;;
(define (Triangle.create !fileIn)

  ;; create instance
  ;; (reading sequentially from model file)
  (defobject Triangle

    ;; read geometry
    (vertexs      (unfoldv 3 #f (lambda (i p) (^read !fileIn))))

    ;; read and condition quality
    (reflectivity (^clamped    ^ZERO ^ONE (^read !fileIn)))
    (emitivity    (^clampedMin ^ZERO      (^read !fileIn)))))




(defclass Triangle

;; queries ------------------------------------------------------------------ ;;

  ;; Axis-aligned bounding box of triangle.
  ;;
  ;; @return [pair[Vector3r Vector3r]] lower and upper corners
  ;;
  (method (bound self)

    ;; calculate min and max across all vertexs
    (let ((lowUpp (let ((reduceVerts (lambda (f)
                                       (f (self 'vertex_ 0)
                                          (f (self 'vertex_ 1)
                                             (self 'vertex_ 2))))))
                    (cons (reduceVerts ^clampedMax)
                          (reduceVerts ^clampedMin)))))

      ;; enlarge by tolerance amount
      (let ((t (^* ^ONE TOLERANCE)))
        (cons (^- (car lowUpp) t)
              (^+ (cdr lowUpp) t)))))
      ;; enlarge with some padding (proportional and fixed)
      ;; (the proportional part allows triangles with large coords to
      ;; still have some padding in single-precision FP)
      ;(let ((nudge (lambda (inc bound)
      ;               (inc bound (^+ (^* ^ONE TOLERANCE)
      ;                              (^* (^abs bound) EPSILON_))))))
      ;  (cons (nudge ^- (car lowUpp))
      ;        (nudge ^+ (cdr lowUpp))))))


  ;; Intersection point of ray with triangle.
  ;;
  ;; @param  rayOrigin    [Vector3r] ray start point
  ;; @param  rayDirection [Vector3r] ray direction (unitized)
  ;; @return [real|boolean] distance along ray, or #f if no intersection
  ;;
  (method (intersection self rayOrigin rayDirection)

    ;; make vectors for two edges sharing vert0
    (let ((edge3 (self 'edge3_))
          (edge0 (self 'edge_ 0)))

      ;; begin calculating determinant -- also used to calculate U parameter
      (let* ((pvec    (^x rayDirection edge3))
             (det     (^. edge0 pvec)))

        ;; if determinant is near zero, ray lies in plane of triangle
        (and (or (<= det (- EPSILON_)) (>= det EPSILON_))

             (let* ((invDet (/ 1.0 det))
                    ;; calculate distance from vertex 0 to ray origin
                    (tvec (^- rayOrigin (self 'vertex_ 0)))
                    ;; calculate U parameter
                    (u (* (^. tvec pvec) invDet)))
               ;; test bounds
               (and (and (>= u 0.0) (<= u 1.0))

                    ;; prepare to, and test V parameter
                    (let* ((qvec (^x tvec edge0))
                           ;; calculate V parameter and test bounds
                           (v (* (^. rayDirection qvec) invDet)))
                      (and (and (>= v 0.0) (<= (+ u v) 1.0))

                           ;; calculate t -- where ray intersects triangle
                           (let ((hitDistance (* (^. edge3 qvec) invDet)))
                             ;; only allow intersections in the forward ray
                             ;; direction
                             (and (>= hitDistance 0.0) hitDistance))))))))))


  ;; Monte-carlo sample point on triangle.
  ;;
  ;; @param  !random [proc[-> real[0-1]]] random number generator
  ;; @return [Vector3r] position
  ;;
  (method (samplePoint self !random)

     ;; get two randoms
     (let ((srr1 (sqrt (!random)))
           (r2   (!random)))

       ;; make barycentric coords
       (let ((c0 (- 1.0 srr1))
             (c1 (* (- 1.0 r2) srr1))
             ;(c2 (* r2 srr1))

             ;; make barycentric axes
             (a0 (self 'edge_ 0))
             (a1 (self 'edge3_)))

         ;; sum scaled components, and offset from corner
         (^+ (^+ (^* a0 c0) (^* a1 c1)) (self 'vertex_ 0)))))


  ;; Normal, unitized.
  ;;
  ;; @return [Vector3r] direction
  ;;
  (method (normal self) (^unitized (self 'normal_)))


  ;; Tangent, unitized.
  ;;
  ;; @return [Vector3r] direction
  ;;
  (method (tangent self) (^unitized (self 'edge_ 0)))


  ;; Area.
  ;; half area of parallelogram (area = magnitude of cross of two edges)
  ;;
  ;; @return [real]
  ;;
  (method (area self) (* 0.5 (^length (self 'normal_))))




;; implementation ----------------------------------------------------------- ;;

  ;; Vertex.
  ;;
  ;; @return [Vector3r]
  ;;
  (method (vertex_ self i) (vector-ref (self 'vertexs) i))


  ;; Edge.
  ;;
  ;; @return [Vector3r]
  ;;
  (method (edge_ self i)

    (^- (self 'vertex_ (modulo (+ i 1) 3))
        (self 'vertex_ (modulo i 3))))


  ;; Edge 3.
  ;;
  ;; @return [Vector3r]
  ;;
  (method (edge3_ self) (^neg (self 'edge_ 2)))


  ;; Normal vector, unnormalised.
  ;;
  ;; @return [Vector3r]
  ;;
  (method (normal_ self) (^x (self 'edge_ 0) (self 'edge_ 1)))

)




;; constants ---------------------------------------------------------------- ;;

;; General tolerance of 1 mm seems reasonable.
;;
(define TOLERANCE (/ 1.0 1024.0))


;; Epsilon suitable for at least single precision.
;;
(define EPSILON_ (/ 1.0 1048576.0))
