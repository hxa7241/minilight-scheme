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
;; * spatialindex




;; Collection of objects in the environment.
;;
;; Constant.
;;
;; @invariants
;; * skyEmission      is a Vector3r >= 0
;; * groundReflection is a Vector3r >= 0 and <= 1
;; * triangles is a vector of Triangles, with length >= 0 and < MAX_TRIANGLES_
;; * emitters  is a vector of Triangles, with length >= 0 and < MAX_TRIANGLES_
;; * spatialIndex is a SpatialIndex




;; initialisation ----------------------------------------------------------- ;;

;; Construct a Scene.
;;
;; @param  !fileIn     [port]     model input file
;; @param  eyePosition [Vector3r] position of eye point
;; @return [Scene]
;;
(define (Scene.create !fileIn eyePosition)

  ;; create instance
  ;; (reading sequentially from model file)
  (defobject Scene

    ;; read and condition background sky and ground values
    (skyEmission      (^clampedMin ^ZERO      (^read !fileIn)))
    (groundReflection (^clamped    ^ZERO ^ONE (^read !fileIn)))

    ;; read objects, until end of file or until maximum reached
    (triangles        (list->vector (let readTriangles ((i 0) (ts '()))
                                      (if (or (>= i MAX_TRIANGLES_)
                                              (not (skipBlanks !fileIn)))
                                        ts
                                        (readTriangles
                                          (+ 1 i)
                                          (cons (Triangle.create !fileIn)
                                                ts))))))
    ;; find emitting objects
    (emitters         (filterv triangles
                               (lambda (t)
                                 ;; having non-zero emission and area
                                 (and (not (^zero? (t 'emitivity)))
                                      (positive?   (t 'area))))))
    ;; make index of objects
    (spatialIndex     (SpatialIndex.create eyePosition triangles))))




(defclass Scene

;; queries ------------------------------------------------------------------ ;;

  ;; Find nearest intersection of ray with object.
  ;;
  ;; @param  rayOrigin    [Vector3r]           ray origin
  ;; @param  rayDirection [Vector3r]           ray direction (unitized)
  ;; @param  lastHit      [Triangle|boolean]   previous intersected object,
  ;;                                           or #f
  ;; @return [pair[Triangle Vector3r]|boolean] triangle and position, or #f
  ;;
  (method (intersection self rayOrigin rayDirection lastHit)

    ((self 'spatialIndex) 'intersection rayOrigin rayDirection lastHit #f))


  ;; Monte-carlo sample point on monte-carlo selected emitting object.
  ;;
  ;; @param  !random [proc[-> real[0-1]]] random number generator
  ;; @return [pair[Triangle Vector3r]|boolean] triangle and position, or #f
  ;;
  (method (emitter self !random)

    ;; only if there are any emitters
    (and (positive? (self 'emittersCount))

         (let* (
                ;; select emitter, by uniform random
                ;; (has small numerical error in distribution if emittersCount
                ;; is not a power of 2, of order emittersCount * 2^-32)
                (emitter  (vector-ref (self 'emitters)
                                      (let ((randomIndex
                                              (inexact->exact
                                                (floor (* (self 'emittersCount)
                                                          (!random)))))
                                            (maxIndex
                                              (- (self 'emittersCount) 1)))
                                        (min randomIndex maxIndex))))
                ;; choose position on triangle, by uniform random
                (position (emitter 'samplePoint !random)))
           (cons emitter position))))


  ;; Number of emitters in scene.
  ;;
  ;; @return [integer]
  ;;
  (method (emittersCount self)

    (vector-length (self 'emitters)))


  ;; Default/'background' light of scene universe.
  ;;
  ;; @param  backDirection [Vector3r] direction to eye (unitized)
  ;; @return [Vector3r] radiance
  ;;
  (method (defaultEmission self backDirection)

    ;; sky for downward ray, ground for upward ray
    (if (negative? (^i backDirection 1))
      (self 'skyEmission)
      (^* (self 'skyEmission) (self 'groundReflection))))

)




;; constants ---------------------------------------------------------------- ;;

;; Maximum number of objects in Scene.
;;
;; (2^24 ~= 16 million)
;;
(define MAX_TRIANGLES_ #x1000000)
