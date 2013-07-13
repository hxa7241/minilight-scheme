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
;; * raytracer
;; * image




;; View definition and rasterizer.
;;
;; Constant.
;;
;; @invariants
;; * viewPosition is a Vector3r
;; * viewAngle is a real >= VIEW_ANGLE_MIN_ and <= VIEW_ANGLE_MAX_ degrees,
;;   in radians
;; * viewDirection is a Vector3r and unitized
;; * right is a Vector3r and unitized
;; * up is a Vector3r and unitized
;; * viewDirection, right, and up form a coordinate frame




;; initialisation ----------------------------------------------------------- ;;

;; Construct a Camera.
;;
;; @param  !fileIn [port] model input file
;; @return [Camera]
;;
(define (Camera.create !fileIn)

  ;; read and condition view definition
  (let* (
         ;; read sequentially from model file
         (viewPosition  (^read !fileIn))
         (viewDirection (let ((vd (^unitized (^read !fileIn))))
                          ;; if degenerate, default to Z
                          (if (^zero? vd) (^create 0.0 0.0 1.0) vd)))
         (viewAngle     (let ((va (readTyped !fileIn real?)))
                          ;; clamp and convert to radians
                          (* (max VIEW_ANGLE_MIN_ (min VIEW_ANGLE_MAX_ va))
                             (/ PI 180.0)))))

    ;; make other directions of view coord frame
    (let ((rightUp
      ;; make trial 'right', using viewDirection and assuming 'up' is Y
      (let ((right (^unitized (^x (^create 0.0 1.0 0.0) viewDirection))))
        ;; check 'right' is valid
        ;; -- i.e. viewDirection was not co-linear with 'up'
        (if (not (^zero? right))

          ;; use 'right', and make 'up' properly orthogonal
          (cons right (^unitized (^x viewDirection right)))

          ;; else, assume a different 'up' and redo
          (let (
                ;; 'up' is Z if viewDirection is down, otherwise -Z
                (up (^create 0.0 0.0 (if (negative? (^i viewDirection 1))
                                       1.0 -1.0))))
            ;; remake 'right'
            (cons (^unitized (^x up viewDirection)) up))))))

      ;; create instance
      (defobject Camera
        (viewPosition  viewPosition)
        (viewAngle     viewAngle)
        (viewDirection viewDirection)
        (right         (car rightUp))
        (up            (cdr rightUp))))))




(defclass Camera

;; queries ------------------------------------------------------------------ ;;

  ;; Position of the eye.
  ;;
  ;; @return [Vector3r]
  ;;
  (method (eyePoint self)

    (self 'viewPosition))


  ;; Accumulate a frame of samples to the image.
  ;;
  ;; @param  camera  [Camera]             camera to render with
  ;; @param  scene   [Scene]              scene to render
  ;; @param  !image  [Image]              image to add to
  ;; @param  !random [proc[-> real[0-1]]] random number generator
  ;; @return [undefined]
  ;;
  (method (frame self scene !random !image)

    ;; make ray tracer
    (let ((rayTracer (RayTracer.create scene))

          ;; get image size
          (width  (!image 'width ))
          (height (!image 'height)))

      ;; step through image pixels
      (do ((y 0 (+ y 1))) ((>= y height))
        (do ((x 0 (+ x 1))) ((>= x width))

          ;; get radiance returning in sample direction from ray tracer
          (let ((radiance

                  ;; make sample ray direction, stratified by pixels
                  (let ((sampleDirection

                          ;; make image plane XY displacement vector [-1,+1)
                          ;; coefficients, with sub-pixel jitter
                          (let ((cx (- (/ (* (+ x (!random)) 2.0) width ) 1.0))
                                (cy (- (/ (* (+ y (!random)) 2.0) height) 1.0)))

                            ;; make image plane offset vector,
                            ;; by scaling the view definition by the
                            ;; coefficients
                            (let ((offset (^+ (^* (self 'right) cx)
                                              (^* (self 'up)
                                                  (* cy (/ height width))))))

                              ;; add image offset vector to view direction
                              (^unitized (^+ (self 'viewDirection)
                                             (^* offset
                                                 (tan (* (self 'viewAngle)
                                                         0.5)))))))))

                    ;; trace ray
                    (rayTracer 'radiance (self 'viewPosition)
                                         sampleDirection !random #f))))
            ;; add radiance to image
            (!image 'addToPixel! x y radiance))))))

)




;; constants ---------------------------------------------------------------- ;;

;; View angle max and min.
;;
(define VIEW_ANGLE_MIN_  10.0)
(define VIEW_ANGLE_MAX_ 160.0)
