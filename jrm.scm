#! /usr/bin/env -S chibi-scheme -r -A ./ -I ${modules}/schemeR7RS

(import (chibi) (chibi match)
	(lib misc)
	(scheme cxr) (only (scheme base) symbol=?)
	)

(define-syntax nth-value
	(syntax-rules ()
		((nth-value n values-producing-form)
			(call-with-values (lambda () values-producing-form)
				(lambda all-values (list-ref all-values n))) )) )

(define (main args)
	(dspl (nth-value 2 (values 'hello 'fuck 'piss 42 43 41)))
	(dsp ""))
	
