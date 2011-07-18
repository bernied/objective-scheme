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

;;; Define Class and Object which require a loop to define.
;;; Class is a subclass of Object. Class is an instance of class.
;;; Object is an instance of Class.
;;; We require Class and Object to exist at the root of the MOP.
(define (obj-make-empty-class-dictionary)
  (obj-dict-init (list
                  (cons 'ivdefs (obj-make-dictionary))
                  (cons 'mdefs (obj-make-dictionary))
                  (cons 'parents '())
                  (cons 'ivs (obj-make-dictionary))
                  (cons 'mtab (obj-make-dictionary)))))

(define (obj-make-dispatcher root-dict class-dict)
  (letrec (
          (get-method
            (lambda (cls msg)
              (obj-dict-get (obj-dict-get (cls 'get-class-dict) 'mtab) msg)))
          (Object
            (lambda (msg . args)
              (let ((method (get-method (obj-dict-get root-dict 'class) msg)))
                (if (eq? method 'nil)
                  (case msg
                    ('get-root-dict root-dict)
                    ('get-class-dict class-dict)
                    ('get-class (obj-dict-get root-dict 'class))
                    (else (error "UNDEFINED METHOD:" msg 'FOR Object)))
                  (apply method Object args)))))
          )
    Object))

(define (obj-init)
  (let* (
        (Class-Prototype
          (obj-make-empty-class-dictionary))
        (Method-Defs
          (obj-make-dictionary))
        (Class-Dictionary
          (obj-dict-init (list
                          (cons 'ivdefs   Class-Prototype)
                          (cons 'ivs      Class-Prototype)
                          (cons 'parents  '())
                          (cons 'mdefs    Method-Defs)
                          (cons 'mtab     Method-Defs))))
        (make-instance
          (lambda (class)
            (let* ((parent-class-dict
                    (obj-dict-get (class 'get-root-dict) class))
                   (class-dict 
                    (obj-dict-recursive-merge!
                      (obj-make-dictionary) (obj-dict-get parent-class-dict 'ivs)))
                  (root-dict
                    (obj-dict-init (list
                                    (cons 'class class)
                                    (cons class class-dict)))))
              (obj-make-dispatcher root-dict class-dict))))
        (Root-Dictionary (obj-make-dictionary))
        (Class (obj-make-dispatcher Root-Dictionary Class-Dictionary))
        )
      (obj-dict-set! Root-Dictionary 'class Class)
      (obj-dict-set! Root-Dictionary Class Class-Dictionary)
      (obj-dict-set! Class-Dictionary 'parents (list (make-instance Class)))
      (obj-dict-set! Method-Defs 'make-instance make-instance)
      (obj-dict-set! Method-Defs 'get-parents
        (lambda (obj)
          (obj-dict-get Class-Dictionary 'parents)))
      Class))


(define Class (obj-init))
;(define Object (car (table-ref (table-ref (Class 'get-root-dict) Class) 'parents)))
;(define Object (car (Class 'get-parents)))

