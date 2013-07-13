;------------------------------------------------------------------------------;
;                                                                              ;
;  MiniLight Scheme : minimal global illumination renderer                     ;
;  Harrison Ainsworth / HXA7241 : 2010, 2011, 2013                             ;
;                                                                              ;
;  http://www.hxa.name/minilight                                               ;
;                                                                              ;
;------------------------------------------------------------------------------;


;; Wrappers for platform dependencies.
;; This set for Bigloo 3.5a.


(module minilight)




;; console ------------------------------------------------------------------ ;;

;; Command-line args.
;;
(define ARGV (cdr (command-line)))


;; Ctrl-c/break/SIGINT handling.
;;
(define (withInterruptHandler handlerProc body)
  (signal 2 handlerProc)
  (body))


;; Deliberate exit.
;; (already defined)
;;
;(define (exit value) ...)




;; IO ----------------------------------------------------------------------- ;;

;; Flush display or write.
;;
(define (flushOutput) (flush-output-port (current-output-port)))


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
(define bitxor bit-xor)


;; Bit test.
;;
(define (bit? n index) (not (zero? (bit-and 1 (bit-rsh n index)))))




;; OS ----------------------------------------------------------------------- ;;

;; Unix time
;;
(define timeUnix current-seconds)
