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




;; Yes, it is the 3D vector type!.
;;
;; The usual arithmetic operators, two constants, and IO -- and three extra
;; abstract manipulations.
;;
;; Constant.
;;
;; Implemented as a Scheme vector.
;;
;; @invariants
;; * length is 3
;; * elements are reals




;; initialisation ----------------------------------------------------------- ;;

;; Create instance.
;; [real real real -> vector]
;;
(define (^create x y z)  (vector x y z))


;; Unfold instance.
;; (well, maybe simply generate rather than unfold)
;; [procedure[integer real -> real] -> vector]
;;
(define (^unfold f)  (unfoldv 3 0.0 f))




;; queries ------------------------------------------------------------------ ;;

;; access ;;

;; Indexing.
;; [vector integer -> real]
;;
(define ^i  vector-ref)



;; basic abstract operations ;;

;; 'Map' a vector to another (unary-op).
;; [procedure[real -> real] vector -> vector]
;;
(define (^uop f v)  (vector (f (^i v 0)) (f (^i v 1)) (f (^i v 2))))


;; 'Map'/'Merge' two vectors into third (binary-op).
;; [procedure[real real -> real] vector vector -> vector]
;;
(define (^bop f v0 v1)
  (vector (f (^i v0 0) (^i v1 0))
          (f (^i v0 1) (^i v1 1))
          (f (^i v0 2) (^i v1 2))))


;; Apply (fold), for Vector3r.
;; [procedure[real real real -> real] vector -> real]
;;
(define (^apply f v)  (f (^i v 0) (^i v 1) (^i v 2)))



;; arithmetic ;;

;; Length.
;; [vector -> real]
;;
(define (^length v)  (sqrt (^. v v)))


;; Dot.
;; [vector vector -> real]
;;
(define (^. v0 v1)  (^apply + (^bop * v0 v1)))


;; Negative.
;; [vector -> vector]
;;
(define (^neg v)  (^uop - v))


;; Abs.
;; [vector -> vector]
;;
(define (^abs v)  (^uop abs v))


;; Unitized (normalized).
;; [vector -> vector]
;;
;; * Zero vectors, and vectors of near zero magnitude, return zero vectors.
;; * Vectors of extremely large magnitude return zero vectors.
;;
(define (^unitized v)
  ;; Zero vectors, and vectors of near zero magnitude, produce zero length,
  ;; and (since 1 / 0 is conditioned to 0) ultimately a zero vector result.
  ;; Vectors of extremely large magnitude produce +infinity length, and (since
  ;; 1 / inf is 0) ultimately a zero vector result.
  ;; (Perhaps zero vectors should produce infinite results, but pragmatically,
  ;; zeros are probably easier to handle than infinities.)
  (let ((len (^length v)))
    (if (zero? len) ^ZERO (^* v (/ 1.0 len)))))


;; Cross.
;; [vector vector -> vector]
;;
(define (^x v0 v1)
  (vector (- (* (^i v0 1) (^i v1 2)) (* (^i v0 2) (^i v1 1)))
          (- (* (^i v0 2) (^i v1 0)) (* (^i v0 0) (^i v1 2)))
          (- (* (^i v0 0) (^i v1 1)) (* (^i v0 1) (^i v1 0)))))


;; Add.
;; [vector vector -> vector]
;;
(define (^+ v0 v1)  (^bop + v0 v1))


;; Subtract.
;; [vector vector -> vector]
;;
(define (^- v0 v1)  (^bop - v0 v1))


;; Multiply.
;; [vector vector|real -> vector]
;;
(define (^* v0 b)  (^bop * v0 (if (vector? b) b (vector b b b))))


;; Divide.
;; [vector vector|real -> vector]
;;
(define (^/ v0 b)  (^bop / v0 (if (vector? b) b (vector b b b))))



;; logical ;;

;; Compare to all zeroes.
;; [vector -> boolean]
;;
(define (^zero? v)  (= 0.0 (^i v 0) (^i v 1) (^i v 2)))


;; Clamp between two values.
;; [vector vector vector -> vector]
;;
(define (^clamped lower upper v)  (^bop min upper (^bop max lower v)))


;; Clamp to min.
;; [vector vector -> vector]
;;
(define (^clampedMin lower v)  (^bop max lower v))


;; Clamp to max.
;; [vector vector -> vector]
;;
(define (^clampedMax upper v)  (^bop min upper v))




;; constants ---------------------------------------------------------------- ;;

;; All zeroes.
;; [vector]
;;
(define ^ZERO (vector 0.0  0.0  0.0))


;; All ones.
;; [vector]
;;
(define ^ONE  (vector 1.0  1.0  1.0))




;; io ----------------------------------------------------------------------- ;;

;; Read from file.
;; [port -> vector] (exception-able)
;;
;; input text format:
;;   blank* ( blank* real blank* real blank* real blank* )
;;
(define (^read !fileIn)

  (let ((list3Real? (lambda (v)
                      ;; check is list of three reals
                      (and (list? v)
                           (= (length v) 3)
                           (apply (lambda (a b c) (and a b c))
                                  (map real? v))))))
    ;; read, convert to vector
    (list->vector (readTyped !fileIn list3Real?))))


;; Write to file.
;; [vector port -> undefined]
;;
;; output text format:
;;   (real real real)
;;
(define (^write v !fileOut)

  (displaymp !fileOut (vector->list v)))
