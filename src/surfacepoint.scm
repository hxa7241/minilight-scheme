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




;; Surface point at a ray-object intersection.
;;
;; All direction parameters are away from surface.
;;
;; Constant.
;;
;; @invariants
;; * triangle is a Triangle
;; * position is a Vector3r




;; initialisation ----------------------------------------------------------- ;;

;; Construct a SurfacePoint.
;;
;; @param  triangle [Triangle] surface's object
;; @param  position [Vector3r] position of point on surface
;; @return [SurfacePoint]
;;
(define (SurfacePoint.create triangle position)

  ;; create instance
  (defobject SurfacePoint
    (triangle triangle)
    (position position)))




(defclass SurfacePoint

;; queries ------------------------------------------------------------------ ;;

  ;; Emission from surface element to position.
  ;;
  ;; @param  toPosition   [Vector3r] point being illuminated
  ;; @param  outDirection [Vector3r] direction (unitized) from emitting point
  ;; @param  solidAngle?  [boolean]  use solid angle
  ;; @return [Vector3r] emitted radiance
  ;;
  (method (emission self toPosition outDirection solidAngle?)

    ;; emit from front face of surface only
    (let ((cos*out (^. outDirection ((self 'triangle) 'normal))))
      (if (positive? cos*out)

        ;; calculate emission
        (let ((emitivity  ((self 'triangle) 'emitivity))
              ;; estimate solid angle
              (solidAngle (if solidAngle?
                            (let ((area       ((self 'triangle) 'area))
                                  (distance^2 (let ((ray (^- toPosition
                                                             (self 'position))))
                                                (^. ray ray))))
                              ;; with infinity clamped-out
                              (/ (* cos*out area) (max distance^2 1e-6)))
                            1.0)))
          (^* emitivity solidAngle))

        ;; no emission
        ^ZERO)))


  ;; Light reflection by surface from ray to ray.
  ;;
  ;; @param  inDirection  [Vector3r] negative of inward ray direction (unitized)
  ;; @param  inRadiance   [Vector3r] inward radiance
  ;; @param  outDirection [Vector3r] outward (eyeward) ray direction (unitized)
  ;; @return [Vector3r] reflected radiance
  ;;
  (method (reflection self inDirection inRadiance outDirection)

    (let ((normal ((self 'triangle) 'normal)))

      ;; calculate projections of directions
      (let ((inDot  (^. inDirection  normal))
            (outDot (^. outDirection normal)))
        ;; check directions are on same side of surface
        (if (positive? (* inDot outDot))

          ;; calculate reflection
          (let ((reflectivity ((self 'triangle) 'reflectivity)))
            ;; ideal diffuse BRDF:
            ;; radiance scaled by reflectivity, cosine, and 1/pi
            (^* (^* inRadiance reflectivity) (/ (abs inDot) PI)))

          ;; transmission not allowed
          ^ZERO))))


  ;; Monte-carlo direction of reflection from surface.
  ;;
  ;; @param  inDirection [Vector3r]           eyeward ray direction (unitized)
  ;; @param  !random     [proc[-> real[0-1]]] random number generator
  ;; @return [pair[Vector3r Vector3r]|boolean] sceneward ray direction
  ;;                                           (unitized) and light scaling of
  ;;                                           interaction point, or #f
  ;;
  (method (nextDirection self inDirection !random)

    ;; calculate reflectivity 'magnitude'
    (let* ((reflectivity     ((self 'triangle) 'reflectivity))
           (reflectivityMean (/ (^. reflectivity ^ONE) 3.0)))

      ;; do russian-roulette for reflectivity 'magnitude'
      (and (< (!random) reflectivityMean)

           ;; cosine-weighted importance sample hemisphere
           (let ((p2r1 (* PI 2.0 (!random)))
                 (sr2  (sqrt (!random))))

             ;; make coord frame and coefficients
             (let (
                   ;; make coord frame coefficients (z in normal direction)
                   (x (* (cos p2r1) sr2))
                   (y (* (sin p2r1) sr2))
                   (z (sqrt (- 1.0 (* sr2 sr2))))

                   ;; make coord frame (except third)
                   (tangent ((self 'triangle) 'tangent))
                   (normal  (let ((n ((self 'triangle) 'normal)))
                              ;; put normal on inward ray side of surface
                              ;; (preventing transmission)
                              (if (negative? (^. n inDirection)) (^neg n) n))))

               ;; make vector and color
               (let (
                     ;; make vector from frame times coefficients
                     (outDirection (^+     (^* tangent             x)
                                       (^+ (^* (^x normal tangent) y)
                                           (^* normal              z))))
                     ;; make color by dividing-out mean from reflectivity
                     (color (^* reflectivity (/ 1.0 reflectivityMean))))

                 ;; disclude degenerate result direction
                 (and (not (^zero? outDirection))
                      (cons outDirection color))))))))


  ;; Rename triangle accessor.
  ;;
  ;; @return [Triangle]
  ;;
  (method (hitObject self)

    (self 'triangle))

)
