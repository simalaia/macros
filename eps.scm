#! /usr/bin/env chibi-scheme

(import (chibi) (chibi match)
	(scheme cxr) (only (scheme base) symbol=?)
	)

(define (expand f)
	(initial-expander f initial-expander) )

(define (initial-expander f e)
	(let ( (r (cond
		          ((symbol? f) *identifier-expander*)
		          ((not (pair? f)) (lambda (f e) f) )
		          ((expander? (car f)) (get (car f) '*EXPANDER*) )
		          (else *application-expander* )) ) )
		(r f e) ))

(define (expand-once f)
	(initial-expander f (lambda (f e) f)) )

(define (*identifier-expander* f e) f)

(define (*application-expander* f e)
	(map (lambda (f) (e f e)) f) )

(define (install-expander kw fn)
	(put kw '*EXPANDER* fn) )

(define (expander? f) (and symbol? f) (get f '*EXPANDER*)) )

(define expanders '())

(define (get n

;; Expand
;; (let ((id exp) ...) body ...)
;; into
;; ((lambda (id ...) body ...) exp ...)
;; I may be doing it wrong...
(install-expander 'let
	(lambda (f e)
		`((lambda ,(map car (cadr x)) ,@(cddr x))
		  ,@(map cadr (cadr x))) ))

(install-expander 'lambda
	(lambda (f e)
		`(lambda ,(cadr f)
		 ,@(map (lambda (f) (e f e)) (cddr f))) ))

(install-expander 'quote (lambda (f e) f))
