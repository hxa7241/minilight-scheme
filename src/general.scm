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




;; Non-project-specific mini library.
;;
;; * exit codes
;; * arithmetical / logical
;; * random numbers
;; * exception handling
;; * IO
;; * comprehensions




;; exit codes --------------------------------------------------------------- ;;

(define SUCCESS_EXIT_CODE 0)
(define FAILURE_EXIT_CODE 1)




;; arithmetical / logical --------------------------------------------------- ;;

;; Pi.

(define PI 3.14159265358979)


;; Log base 10.
;;
;; @param  r [real] number (larger than zero)
;; @return [real]
;;
(define (log10 r) (/ (log r) (log 10.0)))


;; Bitwise xor for real-integers.
;;
;; @param  a [real]
;; @param  b [real]
;; @return [real]
;;
(define (bitxor_ a b)

  (exact->inexact (bitxor (inexact->exact a) (inexact->exact b))))


;; Modulo, for reals.
;;
;; @param  r0 [real]
;; @param  r1 [real]
;; @return [real]
;;
(define (mod r0 r1)

  (let* ((division (/ r0 r1))
         (fraction (- division (floor division))))
    (floor (+ 0.5 (* fraction r1)))))


;; Quotient, for reals.
;;
;; @param  r0 [real]
;; @param  r1 [real]
;; @return [real]
;;
(define (quo r0 r1)

  (truncate (/ r0 r1)))




;; random numbers ----------------------------------------------------------- ;;

;; Create a random number generator producing reals: double-precision, [0,1)
;; interval (never returns 1).
;;
;; @implementation
;;
;; 'Maximally Equidistributed Combined Tausworthe Generators'; L'Ecuyer; 1996.
;; http://www.iro.umontreal.ca/~lecuyer/myftp/papers/tausme2.ps
;; http://www.iro.umontreal.ca/~simardr/rng/lfsr113.c
;;
;; 'Conversion of High-Period Random Numbers to Floating Point'; Doornik; 2006.
;; http://www.doornik.com/research/randomdouble.pdf
;;
;; @invariants
;; * !state is a list of 4 [real] integers >= 0 and < 2^32
;;
;; @return [pair[string, proc[-> real[0-1]]]] pair, of id/seed and generator
;;
;(define (randomGeneratorCreate)
;
; ;; init
; (let ((!state (let ((SEED      987654321.0)
;                     (SEED_MINS (list 2.0 8.0 16.0 128.0)))
;                 ;; Unix time -- signed 32-bit, seconds since 1970
;                 (let* ((time (let ((a (mod (+ (exact->inexact (timeUnix))
;                                               2147483648.0) 4294967296.0)))
;                                ((if (>= a 0.0) - +) a 2147483648.0)))
;                        ;; make unsigned, with 2s-comp bit-pattern
;                        (timeu (if (>= time 0.0) time (+ time 4294967296.0)))
;                        ;; rotate to make frequently changing bits more
;                        ;; significant
;                        (seedt (mod (+ (* timeu 256.0) (quo timeu 16777216.0))
;                                    4294967296.0)))
;                   ;; *** VERY IMPORTANT ***
;                   ;; The initial seeds z1, z2, z3, z4  MUST be larger
;                   ;; than 1, 7, 15, and 127 respectively.
;                   (map (lambda (min) (if (>= seedt min) seedt SEED))
;                        SEED_MINS))))
;
;       ;; constants
;       (masks   (list      2.0         8.0      16.0   128.0))
;       (shifts1 (list 262144.0         4.0     128.0  8192.0))
;       (shifts2 (list     64.0         4.0    8192.0     8.0))
;       (shifts3 (list   8192.0 134217728.0 2097152.0  4096.0)))
;
;   ;; 32-bit unsigned integer generator
;   (let ((!int32u (lambda ()
;                    (set! !state (map (lambda (z m s1 s2 s3)
;                                        (bitxor_ (mod (* (* (quo z m) m) s1)
;                                                      4294967296.0)
;                                                 (quo (bitxor_
;                                                        (mod (* z s2)
;                                                             4294967296.0) z)
;                                                      s3)))
;                                      !state masks shifts1 shifts2 shifts3))
;                    (bitxor_ (bitxor_ (car   !state) (cadr   !state))
;                             (bitxor_ (caddr !state) (cadddr !state))))))
;
;     (cons (number->string (inexact->exact (cadddr !state)) 16)
;           ;; 53-bit precision real builder
;           (lambda ()
;             (let ((int0 (!int32u))
;                   (int1 (!int32u)))
;               (+ (* (if (< int0 2147483648.0) int0 (- int0 4294967296.0))
;                     (/ 1.0 4294967296.0)) 0.5
;                  (* (mod int1 2097152.0) (/ 1.0 9007199254740992.0)))))))))


;; Create a random number generator producing reals: double-precision, [0,1)
;; interval (never returns 1).
;;
;; @implementation
;;
;; 'Maximally Equidistributed Combined Tausworthe Generators'; L'Ecuyer; 1996.
;; http://www.iro.umontreal.ca/~lecuyer/myftp/papers/tausme2.ps
;; http://www.iro.umontreal.ca/~simardr/rng/lfsr113.c
;;
;; 'Conversion of High-Period Random Numbers to Floating Point'; Doornik; 2006.
;; http://www.doornik.com/research/randomdouble.pdf
;;
;; @invariants
;; * !state is a list of 4 [real] integers >= 0 and < 2^32
;;
;; @return [proc[-> real[0-1]]] generator
;;
(define (randomGeneratorCreate)

  ;; init
  (let ((!state (let ((SEED 987654321.0))
                  ;; *** VERY IMPORTANT ***
                  ;; The initial seeds z1, z2, z3, z4  MUST be larger
                  ;; than 1, 7, 15, and 127 respectively.
                  (list SEED SEED SEED SEED)))

        ;; constants
        (masks   (list      2.0         8.0      16.0   128.0))
        (shifts1 (list 262144.0         4.0     128.0  8192.0))
        (shifts2 (list     64.0         4.0    8192.0     8.0))
        (shifts3 (list   8192.0 134217728.0 2097152.0  4096.0)))

    ;; 32-bit unsigned integer generator
    (let ((!int32u (lambda ()
                     (set! !state (map (lambda (z m s1 s2 s3)
                                         (bitxor_ (mod (* (* (quo z m) m) s1)
                                                       4294967296.0)
                                                  (quo (bitxor_
                                                         (mod (* z s2)
                                                              4294967296.0) z)
                                                       s3)))
                                       !state masks shifts1 shifts2 shifts3))
                     (bitxor_ (bitxor_ (car   !state) (cadr   !state))
                              (bitxor_ (caddr !state) (cadddr !state))))))

      ;; 53-bit precision real builder
      (lambda ()
        (let ((int0 (!int32u))
              (int1 (!int32u)))
          (+ (* (if (< int0 2147483648.0) int0 (- int0 4294967296.0))
                (/ 1.0 4294967296.0)) 0.5
             (* (mod int1 2097152.0) (/ 1.0 9007199254740992.0))))))))




;; exception handling ------------------------------------------------------- ;;

;; Minimal exception handling support.
;;
;; Works basically like SRFI-34, but non-nestable -- only one can be active at
;; a time.
;;
;; @param  handler [procedure[any->any]] called if an exception is raised
;; @param  action [procedure[->any]]     to be executed, and allowed to raise
;;                                       an exception
;; @return [any] value returned from action or handler
;;
(define (withExceptionHandler_ handler action)

  ;; receive the result of execution
  (let ((result (call-with-current-continuation
                  (lambda (continuation)
                    ;; setup raise
                    (set! raise_ (lambda (exception)
                                  (continuation (cons #t exception))))
                    (cons #f (action))))))

    ;; if an exception was raised, call handler
    (if (car result)
      (handler (cdr result))
      (cdr result))))


;; Default exception handling is failure exit.
;;
(define raise_ (lambda (_) (exit FAILURE_EXIT_CODE)))




;; IO ----------------------------------------------------------------------- ;;

;; constants
;;
(define CR (integer->char 13))
(define NL (integer->char 10))


;; Read a number of chars.
;;
;; @param  !fileIn [input-port] input file
;; @param  count   [integer]    number of chars to read
;; @return [string] the chars read
;;
(define (readChars !fileIn count)

    (do ((i count (- i 1))
         (c '()   (cons (read-char !fileIn) c)))
        ((<= i 0) (list->string (reverse c)))))


;; Read an item and validate its type.
;;
;; @param  !fileIn [input-port] input file
;; @param  type?   [procedure]  type predicate
;; @return [type] or exception
;;
(define (readTyped !fileIn type?)

  ;; read item
  (let ((item (read !fileIn)))

    ;; validate type
    (if (type? item)
      item
      ;; check for eof
      (raise_ (if (eof-object? item)
               "incomplete input format"
               "invalid input format")))))


;; Move file input pointer past blanks.
;;
;; @param  !fileIn [input-port] input file
;; @return [boolean] false for eof
;;
(define (skipBlanks !fileIn)

  (do ((c (peek-char !fileIn) (peek-char !fileIn)))
      ((or (eof-object? c) (not (char-whitespace? c))) (char? c))
    (read-char !fileIn)))


;; Display plural things.
;;
;; Use 'N for an implementation-dependent 'newline'.
;; Use 'NL for a newline (ASCII 10).
;;
;; @param  args [list[any]] any number of any arguments
;; @return [undefined]
;;
(define (displaym . args)

  (apply displaymp (current-output-port) args))


;; Display plural things, to a port.
;;
;; Use 'N for an implementation-dependent 'newline'.
;; Use 'NL for a newline (ASCII 10).
;;
;; @param  port [output-port] output port
;; @param  args [list[any]]   any number of any arguments
;; @return [undefined]
;;
(define (displaymp port . args)

  (for-each (lambda (arg)
              (if (eq? arg 'N)
                (newline port)
                (display (if (eq? arg 'NL)
                           (integer->char 10)
                           arg)
                         port)))
            args))




;; comprehensions ----------------------------------------------------------- ;;

;; Filter comprehension for vectors.
;;
;; @param  vIn [vector]                    input vector
;; @param  f?  [procedure[any -> boolean]] predicate
;; @return [vector] output vector
;;
(define (filterv vIn f?)

  ;; iterate backward through vector, building list of selected
  (do ((i    (- (vector-length vIn) 1) (- i 1))
       (lOut '()                       (let ((e (vector-ref vIn i)))
                                         (if (f? e) (cons e lOut) lOut))))
      ((negative? i) (list->vector lOut))))


;; Map comprehension for vectors.
;;
;; @param  vIn [vector]                input vector
;; @param  op  [procedure[any -> any]] operation
;; @return [vector] output vector
;;
(define (mapv vIn op)

  (let ((vOut (make-vector (vector-length vIn))))

    ;; iterate through vector, setting new values
    (do ((i (- (vector-length vIn) 1) (- i 1)))
        ((negative? i) vOut)
      (vector-set! vOut i (op (vector-ref vIn i))))))


;; Fold comprehension for vectors.
;;
;; @param  v     [vector]                    input vector
;; @param  init  [any]                       initial value
;; @param  op    [procedure[any any -> any]] operation: sum,item->sum
;; @return [any] output value
;;
(define (foldv v init op)

  ;; iterate through vector, accumulating operation
  (do ((i   (- (vector-length v) 1) (- i 1))
       (sum init                    (op sum (vector-ref v i))))
      ((negative? i) sum)))


;; Unfold comprehension for vectors.
;; (forward)
;;
;; @param  length [integer]                       length
;; @param  init   [any]                           initial value
;; @param  op     [procedure[integer any -> any]] operation: index,prev->val
;; @return [vector] output vector
;;
(define (unfoldv length init op)

  (let ((v (make-vector length)))

    ;; iterate through vector, setting new values
    (do ((i 0    (+ i 1))
         (p init (vector-ref v i)))
        ((>= i length) v)
      (vector-set! v i (op i p)))))
