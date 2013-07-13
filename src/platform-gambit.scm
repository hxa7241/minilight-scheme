;------------------------------------------------------------------------------;
;                                                                              ;
;  MiniLight Scheme : minimal global illumination renderer                     ;
;  Harrison Ainsworth / HXA7241 : 2010, 2011, 2013                             ;
;                                                                              ;
;  http://www.hxa.name/minilight                                               ;
;                                                                              ;
;------------------------------------------------------------------------------;


;; Wrappers for platform dependencies.
;; This set for Gambit 4.6.0.




;; console ------------------------------------------------------------------ ;;

;; Command-line args.
;;
(define ARGV (cdr (command-line)))


;; Ctrl-c/break/SIGINT handling.
;; (unsupported)
;;
(define (withInterruptHandler handlerProc body)
  (body))


;; Deliberate exit.
;; (already defined)
;;
;(define (exit value) ...)




;; IO ----------------------------------------------------------------------- ;;

;; Flush display or write.
;;
(define flushOutput force-output)


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
(define (bit? n index) (bit-set? index n))




;; OS ----------------------------------------------------------------------- ;;

;; Unix time
;;
(define timeUnix (inexact->exact (floor (time->seconds (current-time)))))
