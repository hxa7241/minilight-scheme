;------------------------------------------------------------------------------;
;                                                                              ;
;  MiniLight Scheme : minimal global illumination renderer                     ;
;  Harrison Ainsworth / HXA7241 : 2010, 2011, 2013                             ;
;                                                                              ;
;  http://www.hxa.name/minilight                                               ;
;                                                                              ;
;------------------------------------------------------------------------------;




;; Minimal basic object system.
;;
;; For ordinary standard R5RS Scheme, with no other dependencies.
;; (In only 22 lines.)
;;
;; An alternative to SRFI-9 Records:
;;  * basically a dictionary data structure,
;;  * with fields (for each instance) and methods (shared between instances),
;;  * but no inheritance etc.
;; The main purpose was:
;;  * concise member addressing -- using OO-style instance 'scope/namespace',
;; to cure the pitifully verbose member addressing of records.
;;
;; Example object definition:
;;
;;   ;; constructor
;;   ;;  * using macro: defobject
;;   ;;  * referring to Class1 in the subsequent defclass
;;   ;;  * making an ad hoc set of fields for this particular instance
;;   ;;  * field definitions are evaluated sequentially, as in a let*
;;   (define (Class1.create1 param1 param2)
;;     (defobject Class1
;;       (field1 param1)
;;       (field2 param2)))
;;
;;   ;; methods
;;   ;;  * using macro: defclass
;;   ;;  * making a set of methods to be shared between instances
;;   (defclass Class1
;;     (method (method1 self s)
;;       (string-append "hi " s ", " (self 'field1)))   ;; using self for field
;;     (method (method2 self)
;;       (self 'field2)))
;;
;; Class (i.e. non-instance) fields and methods can just be defined ordinarily,
;; since they do not need to have self fed through.
;;
;; Example instantiation and method call:
;; (must quote method name when calling)
;;
;;   (let ((instance1 (Class1.create1 "where are you?" 42)))
;;     (instance1 'method1 "John"))
;;
;; Limitation:
;; Method parameters with dotted tails are not supported.
;; (perhaps defclass needs another rule ...)
;;
;;
;; Implementation notes:
;;
;; Instead of a type predicate, a type query returning the type name could
;; easily be added: in defclass add a special first member of the list, as a
;; lambda named 'type and returning 'class. (Maybe it could be just a value, not
;; a lambda.)
;;
;; Inheritance could be a straightforward and minimal extension: have defclass
;; take parent parameters, which could then be added as a single list member to
;; its resultant methods list, and have Object.create search those parents if
;; it cannot find the member name in the instance (the fields and methods).
;; That would be the core of it anyway . . .




;; usage interface ---------------------------------------------------------- ;;

;; These mainly neaten the lexical representation a little -- nothing
;; substantial. They core purpose is to define an association-list.


;; Make an instance -- i.e. a set of fields, with a shared set of methods.
;;
(define-syntax defobject
  (syntax-rules ()

    ((_ class (name field) ...)

     ;; sequentialise field definition
     (let* ((name field) ...)
       ;; make association-list of fields
       (let ((fields (list (cons 'name name) ...)))
         ;; make an object from the fields and methods lists
         (Object.create class fields))))))


;; Make a set of methods (shareable between instances).
;;
(define-syntax defclass
  (syntax-rules ()

    ((_ class (method (name param ...) expr1 ...) ...)

     ;; make association-list of methods
     (define class (list (cons 'name (lambda (param ...) expr1 ...)) ...)))))


;; making class fields and methods can be done with normal defines




;; implementation ----------------------------------------------------------- ;;

;; Dispatcher.
;;
(define (Object.create methods fields)

  ;; make and return a lambda closing over the members,
  ;; and make it recursable so that called methods can themselves call it
  (letrec ((self (lambda (name . params)

                   ;; first search fields
                   (let ((field (assq name fields)))
                     (if field
                       (if (null? params)
                         ;; no params: get field
                         (cdr field)
                         ;; params: set field
                         (set-cdr! field (car params)))

                       ;; no field, so search methods
                       (let ((method (assq name methods)))
                         (if method
                           ;; call method
                           (apply (cdr method) (cons self params))

                           ;; nothing found
                           'membernotfound)))))))
    self))
