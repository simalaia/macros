(import (chibi) (chibi match)
  (lib misc)
  (scheme cxr) (only (scheme base) unless when define-record-type)
  (srfi 69)
  )

;; [NOTE]:! It turns out that I had to modify the
;;          expander in order to get gensyms added
;;          to the expansion (instead of only
;;          symbols).

(define (hasheq) (make-hash-table eq? hash-by-identity))
(define (hash-set t k v) (hash-table-set! t k v))
(define (hash-ref t k) (hash-table-ref t k (lambda () #f)))

;; The actual expander
(define expanders (hasheq))
(define (expander? x) (did (hash-ref expanders x)))
(define (install-expander s p) (hash-set expanders s p))
(define-syntax defmac
  (syntax-rules ()
    ((epsmac n b) (install-expander 'n b) )) )
(define (eps-f m) (hash-ref expanders m))

(define (initial-expander x e)
  (match x
    (((? expander? m) . l) ((eps-f m) x e) )
    ((or (? symbol?) (? gensym?)) x )
    (else (map (lambda (x) (e x e)) x) )) )
(define (expand x) (initial-expander x initial-expander))
(define (expand-once x) (initial-expander (lambda (x e) x)))


;; Usage examples
;; No special-form builtins needed
(defmac quote (lambda (x e) x))
(defmac lambda
  (lambda (x e) `(lambda ,(cadr x) ,@(map (lambda (x) (e x e)) (cddr x)))))
(defmac if (lambda (x e) `(if ,@(map (lambda (x) (e x e)) (cdr x)))))
(defmac set! (lambda (x e) `(set! ,(cadr x) ,(e (caddr x) e))))
(defmac aif
  (lambda (x e)
    `(let ((it ,(e (cadr x) e)))
      ,(expand `(if it ,@(cddr x))))))

;; defmacro, as a macro
(define-record-type Gensym (gensym) gensym?)
(define (destruct p a b)
  (match p
    (() b )
    ((or (? symbol?) (? gensym?)) (cons `(,p ,a) b) )
    ((x . y) (destruct x `(car ,a) (destruct y `(cdr ,a) b)) )) )
(define (make-macro p b)
  (let ( (x (gensym)) (e (gensym)) )
    `(lambda (,x ,e)
      (,e (let ,(destruct p `(cdr ,x) '()) ,b) ,e)) ))

(defmac defmacro
  (lambda (x e)
    (let ((k (cadr x)) (p (caddr x)) (b (cadddr x)))
      (e `(install-expander ',k ,(make-macro p b)) e) )) )

;; Currying expanders
(define (application? x) (and (pair? x) (not (expander? (car x)))) )
(defmac curry
  (lambda (x e)
    (let ( (e1 (lambda (x e2)
                (match x ((? symbol?) (e x e2) )
                  (('lambda (a b) c) (dspln c)
                    (e `(lambda (,a) (lambda (,b) ,c)) e2) )
                  ((and (? application?)
                    (a b . c)) (e2 '((,a ,b) ,@c) e2) )
                  (else (e x e2) )) )) )
      (e1 (cadr x) e1) )) )

(define (main args)
  (dspln (expand '(quote x)))
  (dspln (expand '(lambda (x) x)))
  (dspln (expand '(if (quote a) (lambda (b) b) c)))
  (dspln (expand '(aif (quote a) (lambda (b) (set! b c)) c)))
  ;; (dspln (expand '(defmacro let (decls . body) `((lambda (map car decls) ,@body) ,@(map cadr decls))) ))
  (dspln (expand '(curry (lambda (a b) (+ a b)))))

  (dsp ""))
