; "objective-scheme.scm" implementation of MOP for scheme -*-scheme-*-

; This file is an attempt to create an OOP system in scheme using the Metaobject
; Protocol based on
;   "Putting Metaclasses to Work" by Ira Forman and Scott Danforth, 1999.

;;; Some simple assertion code.
(define-macro (assert p)
  (if (eq? p #f) (error "ASSERTION ERROR!") #t))

;;; Definition 1: Dictionary
;;; First we create a dictionary to store slots.
(define (obj-make-dictionary)
  (make-table init: 'nil))

(define (obj-dict-print d)
  (print "root")
  (newline)
  (obj-dict-print-recursive d 1)
  (newline))

(define (obj-dict-print-recursive d n)
  (define (tab n)
    (if (> n 0)
      (begin
        (print "  ")
        (tab (- n 1)))))
  (print "{")
  (newline)
  (table-for-each
    (lambda (k v)
      (if (table? v)
        (begin
          (tab n)
          (print k)
          (newline)
          (tab n)
          (obj-dict-print-recursive v (+ n 1)))
        (begin
          (tab n)
          (print k " = " v ", ")))
        (newline))
    d)
  (tab (- n 1))
  (print "}"))

(define (obj-make-method-table)
  (make-table init: (lambda (obj . args) (error "UNDEFINED METHOD: " obj))))

(define (obj-dict-copy d)
  (table-copy d))
  
(define (obj-dict-deep-copy d)
  (obj-dict-recursive-merge! (obj-make-dictionary) d))

(define (obj-dict? d)
  (table? d))

(define (obj-dict-contains? d k)
  (if (eq? (table-ref d k) 'nil) #f #t))

(define (obj-dict-get d k)
  (table-ref d k))

(define (obj-dict-set! d k v)
  (table-set! d k v)
  d)
  
(define (obj-dict-init list)
  (let ((dictionary (obj-make-dictionary)))
    (for-each
      (lambda (elem)
        (obj-dict-set! dictionary (car elem) (cdr elem)))
      list)
    dictionary))

(define (obj-dict-key-list d)
  (map (lambda (x) (car x)) (table->list d)))

(define (obj-dict-merge! d1 d2)
  (table-merge! d1 d2 #f))

(define (obj-dict-recursive-merge! d1 d2)
  (table-for-each
   (lambda (k v2)
     (let ((v1 (obj-dict-get d1 k)))
       (if (eq? v1 'nil)
           (obj-dict-set! d1 k (if (obj-dict? v2) (obj-dict-copy v2) v2))
           (if (and (obj-dict? v1) (obj-dict? v2))
               (obj-dict-recursive-merge! v1 v2)))))
   d2)
  d1)

;;; Define Class and Object which requires a self-reference to create.
;;; Class is a subclass of Object. Class is an instance of class.
;;; Object is an instance of Class.
;;; We require Class and Object to exist at the root of the MOP.
(define (obj-make-empty-class-dictionary)
  (obj-dict-init (list
                  (cons 'ivdefs (obj-make-dictionary))
                  (cons 'mdefs (obj-make-method-table))
                  (cons 'parents '())
                  (cons 'ivs (obj-make-dictionary))
                  (cons 'mtab (obj-make-method-table)))))

(define (obj-make-dispatcher root-dict class-dict)
  (letrec ((dispatcher
            (lambda (msg . args)
              (case msg
                ('get-class-dict class-dict)
                ('get-root-dict root-dict)
                (else
                  (let*
                    ((class (obj-dict-get root-dict 'class))
                    (parent-class-dict (class 'get-class-dict))
                    (method
                      (obj-dict-get
                        (obj-dict-get
                          (obj-dict-get parent-class-dict 'mtab) class) msg)))
                  (apply method dispatcher args)))))))
    dispatcher))

(define (obj-init-object-methods Object)
  (let 
      ((mdefs
        (obj-dict-get (obj-dict-get (Object 'get-class-dict) 'mdefs) Object))
      (mtab
        (obj-dict-get (obj-dict-get (Object 'get-class-dict) 'mtab) Object))
      (get-class
        (lambda (obj)
          (obj-dict-get (obj 'get-root-dict) 'class)))
      (get-iv
        (lambda (obj var)
          (obj-dict-get (obj 'get-class-dict) var)))
      (set-iv!
        (lambda (obj var value)
          (obj-dict-set! (obj 'get-class-dict) var value)))
      (print-obj
        (lambda (obj)
          (obj-dict-print (obj 'get-root-dict))))
      (get-parents
        (lambda (obj)
          (obj-dict-get (obj 'get-class-dict) 'parents)))
    )
    (obj-dict-set! mdefs 'get-iv get-iv)
    (obj-dict-set! mdefs 'set-iv! set-iv!)
    (obj-dict-set! mdefs 'print print-obj)
    (obj-dict-set! mdefs 'get-class get-class)
    (obj-dict-set! mdefs 'get-parents get-parents)
    (obj-dict-merge! mtab mdefs)))

(define (obj-init-class-methods Class)
  (let 
      ((mdef
        (obj-dict-get (obj-dict-get (Class 'get-class-dict) 'mdefs) Class))
      (make-instance
        (lambda (obj)   ;{"class"=X} <= X.Class."ivs"
          (let*
            ((root-dict
              (obj-dict-merge!
                (obj-dict-init (list (cons 'class obj)))
                (obj-dict-get (obj 'get-class-dict) 'ivs))) ;LAMb: will this work? What is the "class" key in this case?
            (class-dict
              (obj-dict-get root-dict obj)))
            (obj-make-dispatcher root-dict class-dict))))
    )
    (obj-dict-set! mdef 'make-instance make-instance)
    (obj-init-object-methods Class)))

(define (obj-init)
  (let* ((Class-Prototype
          (obj-make-dictionary))
        (Method-Defs
          (obj-make-dictionary))
        (Class-Dictionary
          (obj-dict-init (list
                          (cons 'ivdefs   Class-Prototype)
                          (cons 'ivs      Class-Prototype)
                          (cons 'parents  '())
                          (cons 'mdefs    Method-Defs)
                          (cons 'mtab     Method-Defs))))
        (Root-Dictionary
          (obj-make-dictionary))
        (Class
          (obj-make-dispatcher Root-Dictionary Class-Dictionary)))
      (obj-dict-set! Class-Prototype Class (obj-make-empty-class-dictionary))
      (obj-dict-set! Method-Defs Class (obj-make-method-table))
      (obj-dict-set! Root-Dictionary 'class Class)
      (obj-dict-set! Root-Dictionary Class Class-Dictionary)
      (obj-init-class-methods Class)
      (obj-dict-set! Class-Dictionary 'parents (list (Class 'make-instance)))
      Class))


(define Class (obj-init))
(define Object (Class 'get-parents))
;(define Object (car (table-ref (table-ref (Class 'get-root-dict) Class) 'parents)))

