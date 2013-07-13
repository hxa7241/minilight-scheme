;------------------------------------------------------------------------------;
;                                                                              ;
;  MiniLight Scheme : minimal global illumination renderer                     ;
;  Harrison Ainsworth / HXA7241 : 2010, 2011, 2013                             ;
;                                                                              ;
;  http://www.hxa.name/minilight                                               ;
;                                                                              ;
;------------------------------------------------------------------------------;


;; uses:
;; * vector3r
;; * objects
;; * surfacepoint
;; * scene




;; Ray tracer for general light transport.
;;
;; Traces a path with emitter sampling: A single chain of ray-steps advances
;; from the eye into the scene with one sampling of emitters at each node.
;;
;; Constant.
;;
;; @invariants
;; * scene is a Scene




;; initialisation ----------------------------------------------------------- ;;

;; Construct a RayTracer.
;;
;; @param  scene [Scene] scene to render
;; @return [RayTracer]
;;
(define (RayTracer.create scene)

  ;; create instance
  (defobject RayTracer
    (scene scene)))




(defclass RayTracer

;; queries ------------------------------------------------------------------ ;;

  ;; Radiance returned from a trace.
  ;;
  ;; @param  rayOrigin    [Vector3r]           ray start point
  ;; @param  rayDirection [Vector3r]           ray direction (unitized)
  ;; @param  !random      [proc[-> real[0-1]]] random number generator
  ;; @param  lastHit_     [Triangle|boolean]   previous intersected object,
  ;;                                           or #f
  ;; @return [Vector3r] eye-ward radiance
  ;;
  (method (radiance self rayOrigin rayDirection !random lastHit_)

    ;; intersect ray with scene
    (let ((intersection ((self 'scene) 'intersection rayOrigin rayDirection
                                                     lastHit_)))
      (if intersection
        ;; a hit
        (let (
              ;; make SurfacePoint of intersection
              (surfacePoint (SurfacePoint.create (car intersection)
                                                 (cdr intersection))))
          (let (
                ;; local emission (only for first-hit)
                (localEmission (if (not lastHit_)
                                 (surfacePoint 'emission rayOrigin
                                                         (^neg rayDirection) #f)
                                 ^ZERO))

                ;; emitter sample
                (illumination (or (self 'emitterSample_ rayDirection
                                                        surfacePoint !random)
                                  ^ZERO))

                ;; recursive reflection:
                ;; single hemisphere sample, ideal diffuse BRDF:
                ;;    reflected = (inradiance * pi) * (cos(in) / pi * color) *
                ;;       reflectance
                ;; -- reflectance magnitude is 'scaled' by the russian roulette,
                ;; cos is importance sampled (both done by SurfacePoint),
                ;; and the pi and 1/pi cancel out -- leaving just:
                ;;    inradiance * reflectance color
                (reflection
                  (let ((next (surfacePoint 'nextDirection (^neg rayDirection)
                                                           !random)))
                    ;; check surface reflects ray
                    (if next
                      ;; recurse
                      (let ((direction (car next))
                            (color     (cdr next)))
                        (^* color
                            (self 'radiance (surfacePoint 'position) direction
                                            !random (surfacePoint 'hitObject))))
                      ;; end path
                      ^ZERO))))

          ;; total radiance returned
          (^+ reflection (^+ illumination localEmission))))

        ;; no hit: default/background scene emission
        ((self 'scene) 'defaultEmission (^neg rayDirection)))))




;; implementation ----------------------------------------------------------- ;;

  ;; Radiance from an emitter sample.
  ;;
  ;; @param  rayDirection [Vector3r]           previous ray direction (unitized)
  ;; @param  surfacePoint [SurfacePoint]       surface point receiving emission
  ;; @param  !random      [proc[-> real[0-1]]] random number generator
  ;; @return [Vector3r|boolean] emitted radiance, or #f
  ;;
  (method (emitterSample_ self rayDirection surfacePoint !random)

    ;; single emitter sample, ideal diffuse BRDF:
    ;;    reflected = (emitivity * solidangle) * (emitterscount) *
    ;;       (cos(emitdirection) / pi * reflectivity)
    ;; -- SurfacePoint does the first and last parts (in separate methods)

    ;; check an emitter was found
    (let* ((scene (self 'scene))
           (e     (scene 'emitter !random)))
      (and e
           (let ((emitter      (car e))
                 (emitPosition (cdr e)))

             ;; emit
             (let* (
                    ;; make direction to emit point
                    (emitDirection (^unitized (^- emitPosition
                                                  (surfacePoint 'position))))
                    ;; send shadow ray
                    (i (scene 'intersection (surfacePoint 'position)
                                            emitDirection
                                            (surfacePoint 'hitObject))))

               ;; receive emission (if unshadowed)
               (and (or (not i) (eq? (car i) emitter))
                    (let (
                          ;; get inward emission value
                          (emissionIn
                            ((SurfacePoint.create emitter emitPosition)
                             'emission (surfacePoint 'position)
                             (^neg emitDirection) #t)))
                      ;; get amount reflected by surface
                      (surfacePoint 'reflection emitDirection
                                                (^* emissionIn
                                                    (scene 'emittersCount))
                                                (^neg rayDirection)))))))))

)
