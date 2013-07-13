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




;; Pixel sheet, with simple tone-mapping and file formatting.
;;
;; Uses Ward simple tonemapper:
;; 'A Contrast Based Scalefactor For Luminance Display';
;; Ward;
;; 1994.
;; Graphics Gems 4, AP;
;;
;; Uses PPM image format:
;; http://netpbm.sourceforge.net/doc/ppm.html
;;
;; Mutable.
;;
;; @invariants
;; * width  is an integer >= 1 and <= IMAGE_DIM_MAX_
;; * height is an integer >= 1 and <= IMAGE_DIM_MAX_
;; * pixels is a vector of Vector3r, with length == (width * height)




;; initialisation ----------------------------------------------------------- ;;

;; Construct an Image.
;;
;; @param  !fileIn [port] model input file
;; @return [Image]
;;
(define (Image.create !fileIn)

  ;; create instance
  (defobject Image

    ;; read and condition dimensions
    ;; (sequentially from model file)
    (width  (max 1 (min IMAGE_DIM_MAX_ (readTyped !fileIn integer?))))
    (height (max 1 (min IMAGE_DIM_MAX_ (readTyped !fileIn integer?))))

    ;; allocate storage
    (pixels (make-vector (* width height) ^ZERO))))




(defclass Image

;; commands ---------------------------------------------------------------- ;;

  ;; Accumulate (add, not just assign) a value to the image.
  ;;
  ;; @param  x        [integer]  x coord
  ;; @param  y        [integer]  y coord
  ;; @param  radiance [Vector3r] radiance
  ;; @return [undefined]
  ;;
  (method (addToPixel! self x y radiance)

    ;; only inside image bounds
    (if (and (>= x 0) (< x (self 'width))
             (>= y 0) (< y (self 'height)))

      ;; calculate linear index
      (let ((index (+ x (* (- (self 'height) 1 y) (self 'width)))))
        ;; add radiance to pixel
        (vector-set! (self 'pixels) index
                     (^+ (vector-ref (self 'pixels) index) radiance)))))




;; queries ------------------------------------------------------------------ ;;

  ;; Write the image to a serialised format.
  ;;
  ;; @param  iteration [integer] number of accumulations made to the image
  ;; @param  !fileOut  [port]    file to receive the serialised image
  ;; @return [undefined]
  ;;
  (method (formatted self iteration !fileOut)

    ;; make pixel value accumulation divider
    ;; and tonemapping factor
    (let* ((divider        (/ 1.0 (exact->inexact (max 1 iteration))))
           (tonemapScaling (Image.toneMappingFactor_ (self 'pixels) divider)))

      ;; write PPM P6 format

      ;; write header
      ;; write ID and comment
      (displaymp !fileOut "P6" 'NL "# " MINILIGHT_URI_ 'NL 'NL)
      ;; write width, height, maxval
      (displaymp !fileOut (self 'width) " " (self 'height) 'NL 255 'NL)

      ;; write pixels
      (do ((p 0 (+ p 1))) ((>= p (vector-length (self 'pixels))))
        ;; write channels
        (do ((c 0 (+ c 1))) ((>= c 3))

          (let* ((channel   (^i (vector-ref (self 'pixels) p) c))
                 ;; tonemap, gamma encode, quantize
                 (mapped    (* channel divider tonemapScaling))
                 (gammaed   (expt (max 0.0 mapped) GAMMA_ENCODE_))
                 (quantized (min 255.0 (floor (+ (* gammaed 255.0) 0.5)))))

            ;; output as byte
            (writeByte (inexact->exact quantized) !fileOut))))))

)




;; implementation ----------------------------------------------------------- ;;

;; Calculate tone-mapping scaling factor.
;;
;; @return [real]
;;
(define (Image.toneMappingFactor_ pixels divider)

  ;; calculate estimate of world-adaptation luminance
  ;; as log mean luminance of scene
  (let ((adaptLuminance
          ;; sum
          (let ((sumOfLogs (foldv pixels 0.0
                                  (lambda (sum pixel)
                                    ;; luminance
                                    (let ((y (* (^. pixel RGB_LUMINANCE_)
                                                divider)))
                                      ;; clamp luminance to a perceptual
                                      ;; minimum
                                      (+ sum (log10 (max 1e-4 y))))))))
            ;; mean
            (expt 10.0 (/ sumOfLogs (vector-length pixels))))))

    ;; make scale-factor from:
    ;; ratio of minimum visible differences in luminance, in display-adapted
    ;; and world-adapted perception (discluding the constant that cancelled),
    ;; divided by display max to yield a [0,1] range
    (let ((a (+ 1.219 (expt (* DISPLAY_LUMINANCE_MAX_ 0.25) 0.4)))
          (b (+ 1.219 (expt adaptLuminance                  0.4))))

      (/ (expt (/ a b) 2.5) DISPLAY_LUMINANCE_MAX_))))




;; constants ---------------------------------------------------------------- ;;

;; Image dimension max.
;;
(define IMAGE_DIM_MAX_ 4000)


;; Image file comment.
;;
(define MINILIGHT_URI_ "http://www.hxa.name/minilight")


;; Guess of average screen maximum brightness.
;;
(define DISPLAY_LUMINANCE_MAX_ 200.0)


;; ITU-R BT.709 standard RGB luminance weighting.
;;
(define RGB_LUMINANCE_ (^create 0.2126 0.7152 0.0722))


;; ITU-R BT.709 standard gamma.
;;
(define GAMMA_ENCODE_ 0.45)
