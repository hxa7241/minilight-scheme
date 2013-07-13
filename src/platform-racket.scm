;------------------------------------------------------------------------------;
;                                                                              ;
;  MiniLight Scheme : minimal global illumination renderer                     ;
;  Harrison Ainsworth / HXA7241 : 2010, 2011, 2013                             ;
;                                                                              ;
;  http://www.hxa.name/minilight                                               ;
;                                                                              ;
;------------------------------------------------------------------------------;


;; Wrappers for platform dependencies.
;; This set for Racket 5.3.3.


#lang r5rs

(#%require (only racket/base
  current-command-line-arguments with-handlers exn:break? exit flush-output
  delete-file exn:fail:filesystem? write-byte bitwise-xor bitwise-bit-set?
  current-seconds))




;; console ------------------------------------------------------------------ ;;

;; Command-line args.
;;
(define ARGV (vector->list (current-command-line-arguments)))


;; Ctrl-c/break/SIGINT handling.
;;
(define (withInterruptHandler handlerProc body)
  (with-handlers ((exn:break? handlerProc)) (body)))


;; Deliberate exit.
;; (already defined)
;;
;(define (exit value) ...)




;; IO ----------------------------------------------------------------------- ;;

;; Flush display or write.
;;
(define flushOutput flush-output)


;; Delete a file.
;;
(define (deleteFile pathName)
  (with-handlers
    ((exn:fail:filesystem? (lambda (_) #t)))
    (delete-file pathName)))


;; Output byte.
;;
(define writeByte write-byte)




;; bitwise arithmetic ------------------------------------------------------- ;;

;; Bitwise xor (integer).
;;
(define bitxor bitwise-xor)


;; Bit test.
;;
(define bit? bitwise-bit-set?)




;; OS ----------------------------------------------------------------------- ;;

;; Unix time
;;
(define timeUnix current-seconds)
