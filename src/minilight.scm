;------------------------------------------------------------------------------;
;                                                                              ;
;  MiniLight Scheme : minimal global illumination renderer                     ;
;  Harrison Ainsworth / HXA7241 : 2010, 2011, 2013                             ;
;                                                                              ;
;  http://www.hxa.name/minilight                                               ;
;                                                                              ;
;------------------------------------------------------------------------------;


;; uses:
;; * platform-...
;; * general
;; * image
;; * camera
;; * scene




;; Control-module and entry point.
;;
;; Handles command-line UI, and runs the main progressive-refinement render
;; loop.
;;
;; Usage: Supply a model file pathname as the command-line argument. Or -? for
;; help.




;; user messages ------------------------------------------------------------ ;;

;; elements
;;

(define TITLE  "MiniLight 1.6 Scheme")
(define AUTHOR "Harrison Ainsworth / HXA7241 : 2010, 2011, 2013")
(define URL    "http://www.hxa.name/minilight")
(define DATE   "2013-05-04")

(define LINE
  (list
    "----------------------------------------------------------------------"
    'N))

(define DESCRIPTION "MiniLight is a minimal global illumination renderer.")

(define USAGE
  (list
    "usage:" 'N
    "  minilight modelFilePathName" 'N
    'N
    "The model text file format is:" 'N
    "  #MiniLight" 'N
    'N
    "  iterations" 'N
    'N
    "  imagewidth imageheight" 'N
    "  viewposition viewdirection viewangle" 'N
    'N
    "  skyemission groundreflection" 'N
    'N
    "  vertex0 vertex1 vertex2 reflectivity emitivity" 'N
    "  vertex0 vertex1 vertex2 reflectivity emitivity" 'N
    "  ..." 'N
    'N
    "- where iterations and image values are integers, viewangle is a real," 'N
    "and all other values are three parenthised reals. The file must end" 'N
    "with a newline. E.g.:" 'N))

(define EXAMPLE
  (list
    "  #MiniLight" 'N
    'N
    "  100" 'N
    'N
    "  200 150" 'N
    "  (0 0.75 -2) (0 0 1) 45" 'N
    'N
    "  (3626 5572 5802) (0.1 0.09 0.07)" 'N
    'N
    "  (0 0 0) (0 1 0) (1 1 0)  (0.7 0.7 0.7) (0 0 0)" 'N
    'N))


;; compounds
;;

(define BANNER_MESSAGE
  `(N "  " ,TITLE " - " ,URL N N))

(define HELP_MESSAGE
  `(N ,@LINE "  " ,TITLE N N "  " ,AUTHOR N "  " ,URL N N "  " ,DATE N ,@LINE
    N ,DESCRIPTION N N ,@USAGE ,@EXAMPLE))




;; constants ---------------------------------------------------------------- ;;

(define MODEL_FORMAT_ID "#MiniLight")




;; implementation ----------------------------------------------------------- ;;

;; Render iteratively.
;;
;; @param  imageFilePathName [string]             image file name
;; @param  iterations        [integer]            number of iterations
;; @param  !image            [Image]              image to accumulate to
;; @param  camera            [Camera]             camera to render with
;; @param  scene             [Scene]              scene to render
;; @param  !random           [proc[-> real[0-1]]] random number generator
;; @return [undefined]
;;
(define (render imageFilePathname iterations !image camera scene !random)

  ;; catch/handle ctrl-c/break/sigint
  (withInterruptHandler
    (lambda (signalNumber)
      (displaym 'N "interrupted" 'N)
      (exit))

    (lambda () 
      ;; render by progressive refinement
      (do ((frameNo 1 (+ frameNo 1))) ((> frameNo iterations))
        ;; display current iteration number
        (displaym CR "iteration: " frameNo)
        (flushOutput)
      
        ;; render a frame
        (camera 'frame scene !random !image)
      
        ;; save image, at 2 times error-halving rate, and at start and end
        (if (or (= 1 frameNo)
                (let ((nlog2 (lambda (r)
                                (floor (/ (log r) (log 2.0))))))
                                ;(floor (* 2.0 (/ (log r) (log 2.0)))))))
                  (> (nlog2 frameNo) (nlog2 (- frameNo 1))))
                (= frameNo iterations))
          (begin
            ;; Scheme might fail if asked to write to an existing file, so
            ;; delete first
            (deleteFile imageFilePathname)
            ;; open file, write image frame to file, close file
            (call-with-output-file imageFilePathname
                                   (lambda (!fileOut)
                                     (!image 'formatted frameNo !fileOut))))))
      
      (displaym 'N "finished" 'N))))


;; Prepare to render.
;;
;; @param  argv [list[string]] command-line args
;; @return [undefined]
;;
(define (execute argv)

  ;; make random generator
  (let* ((!random (randomGeneratorCreate)))

    ;; make model filename and open model file
    (let* ((modelFilePathname (car argv))
           (!modelFile        (open-input-file modelFilePathname)))

      ;; check model file format identifier at start of first line
      (if (not (and (string=? (readChars !modelFile
                                         (string-length MODEL_FORMAT_ID))
                              MODEL_FORMAT_ID)
                    (let ((c (peek-char !modelFile)))
                      (or (char=? c NL) (char=? c CR)))))
        (raise_ "unrecognised input format"))

      ;; make rendering objects
      ;; (reading sequentially from model file)
      (let* ((iterations (readTyped     !modelFile integer?))
             (!image     (Image.create  !modelFile))
             (camera     (Camera.create !modelFile))
             (scene      (Scene.create  !modelFile (camera 'eyePoint))))

        ;; close model file
        (close-input-port !modelFile)

        ;; make output filename
        (let ((imageFilePathname (string-append modelFilePathname ".ppm")))

          ;; start rendering
          (render imageFilePathname iterations !image camera scene !random))))))




;; entry point -------------------------------------------------------------- ;;

;; catch/handle all exceptions
(withExceptionHandler_

  (lambda (exception)
    (displaym 'N "*** execution failed:  " exception 'N)
    (exit FAILURE_EXIT_CODE))

  (lambda ()
    ;; check for help request
    (if (or (null? ARGV)
            (equal? (car ARGV) "-?")
            (equal? (car ARGV) "--help"))

      ;; print help, and finish
      (apply displaym HELP_MESSAGE)

      ;; execute
      (begin
        (apply displaym BANNER_MESSAGE)
        (execute ARGV)))))
