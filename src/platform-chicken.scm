;------------------------------------------------------------------------------;
;                                                                              ;
;  MiniLight Scheme : minimal global illumination renderer                     ;
;  Harrison Ainsworth / HXA7241 : 2010, 2011, 2013                             ;
;                                                                              ;
;  http://www.hxa.name/minilight                                               ;
;                                                                              ;
;------------------------------------------------------------------------------;


;; Wrappers for platform dependencies.
;; This set for ChickenScheme 4.6.0.


(require-extension posix)




;; console ------------------------------------------------------------------ ;;

;; Command-line args.
;;
(define ARGV (command-line-arguments))


;; Ctrl-c/break/SIGINT handling.
;;
(define (withInterruptHandler handlerProc body)
  (set-signal-handler! signal/int handlerProc)
  (body))


;; Deliberate exit.
;; (already defined)
;;
;(define (exit value) ...)




;; IO ----------------------------------------------------------------------- ;;

;; Flush display or write.
;;
(define flushOutput flush-output)


;; Delete a file.
;; (existing files can be overwritten, so not needed)
;;
(define (deleteFile _) #f)


;; Output byte.
;;
(define (writeByte b port) (write-char (integer->char b) port))




;; bitwise arithmetic ------------------------------------------------------- ;;

;; Bitwise xor (integer).
;;
(define bitxor bitwise-xor)


;; Bit test.
;;
(define bit? bit-set?)




;; OS ----------------------------------------------------------------------- ;;

;; Unix time
;;
(define timeUnix current-seconds)
