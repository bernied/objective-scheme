;;; @Package     Testeez
;;; @Subtitle    Lightweight Unit Test Mechanism for R5RS Scheme
;;; @HomePage    http://www.neilvandyke.org/testeez/
;;; @Author      Neil Van Dyke
;;; @Version     0.5
;;; @Date        2009-05-28
;;; @PLaneT      neil/testeez:1:3

;; $Id: testeez.ss,v 1.76 2009/05/29 11:42:28 neilpair Exp $

;;; @legal
;;; Copyright @copyright{} 2005--2009 Neil Van Dyke.  This program is Free
;;; Software; you can redistribute it and/or modify it under the terms of the
;;; GNU Lesser General Public License as published by the Free Software
;;; Foundation; either version 3 of the License (LGPL 3), or (at your option)
;;; any later version.  This program is distributed in the hope that it will be
;;; useful, but without any warranty; without even the implied warranty of
;;; merchantability or fitness for a particular purpose.  See
;;; @indicateurl{http://www.gnu.org/licenses/} for details.  For other licenses
;;; and consulting, please contact the author.
;;; @end legal

;;; #lang scheme/base

;;; @section Introduction
;;;
;;; Testeez is a simple lightweight unit test mechanism for R5RS Scheme.  It
;;; was written to support regression test suites embedded within the source
;;; code files of the author's portable Scheme libraries.
;;;
;;; @subsection Example
;;;
;;; A series of Testeez tests is listed within the @code{testeez} syntax.  For
;;; example:
;;;
;;; @lisp
;;; (testeez
;;;  "Foo Station"
;;;
;;;  (test/equal  "Put two and two together" (+ 2 2) 4)
;;;
;;;  (test-define "Bar function" bar (lambda (x) (+ x 42)))
;;;
;;;  (test/equal  "Bar scene" (bar 69) 0)
;;;
;;;  (test/eqv    "Full circle" (* (bar -21) 2) 42)
;;;
;;;  (test/eqv    "Multiple"
;;;               (values (+ 2 2) (string #\h #\i) (char-upcase #\p))
;;;               (values 4 "hi" #\P)))
;;; @end lisp
;;;
;;; When evaluated, output like the following (which looks prettier fontified
;;; in Emacs's @code{*scheme*} buffer) is printed:
;;;
;;; @smallexample
;;; ;;; BEGIN "Foo Station" TESTS
;;;
;;; ;; 1. Put two and two together
;;; (+ 2 2)
;;; ;; ==> 4
;;; ;; Passed.
;;;
;;; ;; DEFINE: Bar function
;;; (define bar (lambda (x) (+ x 42)))
;;;
;;; ;; 2. Bar scene
;;; (bar 69)
;;; ;; ==> 111
;;; ;; FAILED!  Expected:
;;; ;;     0
;;;
;;; ;; 3. Full circle
;;; (* (bar -21) 2)
;;; ;; ==> 42
;;; ;; Passed.
;;;
;;; ;; 4. Multiple
;;; (values (+ 2 2) (string #\h #\i) (char-upcase #\p))
;;; ;; ==> 4
;;; ;;     "hi"
;;; ;;     #\P
;;; ;; Passed.
;;;
;;; ;;; END "Foo Station" TESTS: FAILED
;;; ;;;     (Total: 4  Passed: 3  Failed: 1)
;;; @end smallexample
;;;
;;; @subsection Shorthand
;;;
;;; The @code{testeez} syntax also supports shorthand or abbreviated forms, for
;;; quick interactive use, such as in an editor buffer while rapid-prototyping
;;; a function, and in a REPL while debugging.  For an example of shorthand,
;;; the Scheme expression:
;;;
;;; @lisp
;;; (testeez ((+ 1 2) 3) ((* 6 7) 42))
;;; @end lisp
;;;
;;; @noindent
;;; is equivalent to:
;;;
;;; @lisp
;;; (testeez ""
;;;          (test/equal "" (+ 1 2) 3)
;;;          (test/equal "" (* 6 7) 42))
;;; @end lisp
;;;
;;; Future versions of Testeez will add additional features, such as custom
;;; predicates and handling of errors.
;;;
;;; @subsection Embedding
;;;
;;; By following a simple convention, Testeez tests can be embedded in a Scheme
;;; source file with the code that is tested, while permitting the tests to be
;;; disabled and the dependency on Testeez removed for production code.  For
;;; example, to use Testeez in a ``Foo'' library, one can first add a syntax
;;; wrapper around @code{testeez} like so:
;;;
;;; @example
;;; (define-syntax %foo:testeez
;;;   (syntax-rules ()
;;;     ((_ X ...)
;;;      ;; Note: Comment-out exactly one of the following two lines.
;;;      ;; (error "Tests disabled.")
;;;      (testeez X ...)
;;;      )))
;;; @end example
;;;
;;; Then, this wrapper @code{%foo:testeez} can be used in a procedure that
;;; executes the test suite of the ``Foo'' library:
;;;
;;; @lisp
;;; (define (%foo:test)
;;;   (%foo:testeez
;;;    "Foo Station"
;;;     ....))
;;; @end lisp

;;; @section Interface

;;; The interface consists of the @code{testeez} syntax.

(define (%testeez:make-data title) (vector title 0 0 0))

(define (%testeez:data-title  o) (vector-ref o 0))
(define (%testeez:data-total  o) (vector-ref o 1))
(define (%testeez:data-passed o) (vector-ref o 2))
(define (%testeez:data-failed o) (vector-ref o 3))

(define (%testeez:set-data-title!  o x) (vector-set! o 0 x))
(define (%testeez:set-data-total!  o x) (vector-set! o 1 x))
(define (%testeez:set-data-passed! o x) (vector-set! o 2 x))
(define (%testeez:set-data-failed! o x) (vector-set! o 3 x))

(define (%testeez:print-values-list first-prefix next-prefix val-list)
  (display first-prefix)
  (if (null? val-list)
      (newline)
      (let loop ((val-list val-list))
        (write (car val-list))
        (newline)
        (or (null? (cdr val-list))
            (begin (display next-prefix)
                   (loop (cdr val-list)))))))

(define (%testeez:print-result result-list)
  (%testeez:print-values-list ";; ==> "
                              ";;     "
                              result-list))

(define (%testeez:start-test data desc expr-quoted)
  (%testeez:set-data-total! data (+ 1 (%testeez:data-total data)))
  (newline)
  (display ";; ")
  (display (%testeez:data-total data))
  (display ". ")
  (display desc)
  (newline)
  (write expr-quoted)
  (newline))

(define (%testeez:finish-test data pred pred-rest result-list expected-list)
  (let ((failed (lambda ()
                  (%testeez:set-data-failed! data
                                             (+ 1 (%testeez:data-failed data)))
                  (display ";; FAILED!  Expected:")
                  (newline)
                  (%testeez:print-values-list ";;     "
                                              ";;     "
                                              expected-list))))
    (%testeez:print-result result-list)
    (let loop ((pred          pred)
               (pred-rest     pred-rest)
               (result-list   result-list)
               (expected-list expected-list))
      (if (null? result-list)
          (if (null? expected-list)
              (begin (%testeez:set-data-passed!
                      data
                      (+ 1 (%testeez:data-passed data)))
                     (display ";; Passed.")
                     (newline))
              (failed))
          (if (null? expected-list)
              (failed)
              (if (pred (car result-list) (car expected-list))
                  (if (null? pred-rest)
                      (loop pred
                            pred-rest
                            (cdr result-list)
                            (cdr expected-list))
                      (loop (car pred-rest)
                            (cdr pred-rest)
                            (cdr result-list)
                            (cdr expected-list)))
                  (failed)))))))

(define (%testeez:start-eval desc expr-quoted)
  (newline)
  (display ";; EVAL: ")
  (display desc)
  (newline)
  (write expr-quoted)
  (newline))

(define (%testeez:start-define desc expr-quoted)
  (newline)
  (display ";; DEFINE: ")
  (display desc)
  (newline)
  (write expr-quoted)
  (newline))

(define (%testeez:start-tests title)
  (newline)
  (display ";;; BEGIN")
  (and title
       (begin (display " ")
              (write title)))
  (display " TESTS")
  (newline)
  (%testeez:make-data title))

(define (%testeez:finish-tests data)
  (let ((total  (%testeez:data-total  data))
        (passed (%testeez:data-passed data))
        (failed (%testeez:data-failed data)))
    ;; TODO: Check that total = passed + failed
    (newline)
    (display ";;; END")
    (let ((title (%testeez:data-title data)))
      (and title
           (begin (display " ")
                  (write title))))
    (display " TESTS: ")
    (display (cond ((zero? failed) "PASSED")
                   ;; ((zero? passed) "ALL FAILED")
                   (else           "FAILED")))
    (newline)
    (display ";;;     (Total: ")
    (display total)
    (display "  Passed: ")
    (display passed)
    (display "  Failed: ")
    (display failed)
    (display ")")
    (newline)))

;;; @defsyntax testeez [ title ] form ...
;;;
;;; The @code{testeez} syntax contains a short string @var{title} and one or
;;; more @var{forms}, of the following syntaxes, which are evaluated in order.
;;;
;;; @table @code
;;;
;;; @item (test/equal @var{desc} @var{expr} @var{expected})
;;; Execute a test case.  @var{desc} is a short title or description of the
;;; test case, @var{expr} is a Scheme expression, and @var{expected} is an
;;; expression for the expected value (or multiple values).  The test case
;;; passes iff each value of @var{expr} is @code{equal?} to the corresponding
;;; value of @var{expected}.
;;;
;;; @item (test/eq @var{desc} @var{expr} @var{expected})
;;; Like @code{test/equal}, except the equivalence predicate is @code{eq?}
;;; rather than @code{equal?}.
;;;
;;; @item (test/eqv @var{desc} @var{expr} @var{expected})
;;; Like @code{test/equal}, except the equivalence predicate is @code{eqv?}
;;; rather than @code{equal?}.
;;;
;;; @item (test-define @var{desc} @var{name} @var{val})
;;; Bind a variable.  @var{desc} is a short description string, @var{name} is
;;; the identifier, and @var{val} is the value expression.  The binding is
;;; visible to the remainder of the enclosing @code{testeez} syntax.
;;;
;;; @item (test-eval @var{desc} @var{expr})
;;; Evaluate an expression.
;;;
;;; @item (@var{expr} @var{expected})
;;; Shorthand for @code{(test/equal "" @var{expr} @var{expected})}.  This
;;; shorthand is intended for interactive and rapid-prototyping use, not for
;;; released code.
;;;
;;; @end table

;; TODO: Lose the "begin"s.

;; TODO: Expose the custom equivalence predicates, once we're sure we like
;; the syntax.  Should add generic predicates first.

(define-syntax %testeez:body
  (syntax-rules (test/eq test/equal test/eqv test-eval test-define)

    ((_ DATA-VAR
        (%testeez:test/equiv DESC EXPR EXPECTED (PRED0 PRED1 ...))
        REST ...)
     ;; TODO: Maybe turn "(PRED0 PRED1 ...)" into a string so that
     ;; "%testeez:finish-test" can report the equivalence predicate(s) used.
     (begin (%testeez:start-test  DATA-VAR DESC (quote EXPR))
            (let ((result-list   (call-with-values (lambda () EXPR)     list))
                  (expected-list (call-with-values (lambda () EXPECTED) list)))
              (%testeez:finish-test DATA-VAR
                                    PRED0
                                    (quasiquote ((unquote PRED1) ...))
                                    result-list
                                    expected-list))
            (%testeez:body        DATA-VAR REST ...)))

    ((_ DATA-VAR (test/eq DESC EXPR EXPECTED) REST ...)
     (%testeez:body DATA-VAR
                    (%testeez:test/equiv DESC EXPR EXPECTED (eq?))
                    REST ...))

    ((_ DATA-VAR (test/equal DESC EXPR EXPECTED) REST ...)
     (%testeez:body DATA-VAR
                    (%testeez:test/equiv DESC EXPR EXPECTED (equal?))
                    REST ...))

    ((_ DATA-VAR (test/eqv DESC EXPR EXPECTED) REST ...)
     (%testeez:body DATA-VAR
                    (%testeez:test/equiv DESC EXPR EXPECTED (eqv?))
                    REST ...))

    ((_ DATA-VAR (test-define DESC NAME VAL) REST ...)
     (begin (%testeez:start-define DESC
                                   (list 'define
                                         (quote NAME)
                                         (quote VAL)))
            (let ()
              (define NAME VAL)
              (%testeez:body DATA-VAR REST ...))))
    ((_ DATA-VAR (test-eval DESC EXPR) REST ...)
     (begin (%testeez:start-eval   DESC (quote EXPR))
            (let ((result (call-with-values (lambda () EXPR) list)))
              (%testeez:print-result result))
            (%testeez:body         DATA-VAR REST ...)))

    ((_             DATA-VAR (              EXPR EXPECTED) REST ...)
     (%testeez:body DATA-VAR (test/equal "" EXPR EXPECTED) REST ...))

    ((_ DATA-VAR) (void))))

(define-syntax testeez
  (syntax-rules (test/equal test-eval test-define)
    ((_ (X ...) BODY ...)
     (testeez #f (X ...) BODY ...))
    ((_ TITLE BODY ...)
     (let ((data (%testeez:start-tests TITLE)))
       (%testeez:body         data BODY ...)
       (%testeez:finish-tests data)))))

;;; @unnumberedsec History

;;; @table @asis
;;;
;;; @item Version 0.5 --- 2009-05-28 --- PLaneT @code{(1 3)}
;;; Added support for void return values.
;;;
;;; @item Version 0.4 --- 2009-03-02 --- PLaneT @code{(1 2)}
;;; License is now LGPL 3.  Minor changes for PLT 4.  Changed to new Scheme
;;; administration system.  There is now a @code{main.ss}.
;;;
;;; @item Version 0.3 --- 2005-05-30 --- PLaneT @code{(1 1)}
;;; Shorthand syntax added.  Minor formatting change to test log output.
;;;
;;; @item Version 0.2 --- 2005-03-07 --- PLaneT @code{(1 0)}
;;; Multiple values are now supported.  @code{test/eq} and @code{test/eqv}
;;; have been added.  Minor formatting changes to test log output.
;;;
;;; @item Version 0.1 --- 2005-01-02
;;; Initial release.
;;;
;;; @end table

;(provide testeez)
