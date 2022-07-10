#lang racket

;TDA - DATE

;CONSTRUCTOR
;DOMINIO: ENTERO X ENTERO X ENTERO
;RECORRIDO: LISTA DE ENTEROS
;CREA UNA LISTA DE 3 ELEMENTOS (ELEMENTO "DATE")

(define createDate
  (lambda (D M Y) (list D M Y)))

;PERTENENCIA
(define date?
  (lambda (D)
    (if (and
         (integer? (car D))
         (not (null? (cdr D)))
         (integer? (cadr D))
         (not (null? (cddr D)))
         (integer? (caddr D))
         (null? (cdddr D)))
        #t
        #f)))

;SELECTOR

(define getDay
  (lambda (D)
    (if (date? D)
        (car D)
        #f)))

(define getMonth
  (lambda (D)
    (if (date? D)
        (cadr D)
        #f)))

(define getYear
  (lambda (D)
    (if (date? D)
        (caddr D)
        #f)))

;MODIFICADOR

(define modDay
  (lambda (D A)
    (if (and (date? D) (integer? A))
        (cons A (cdr D))
        #f)))

(define modMonth
  (lambda (D A)
    (if (and (date? D) (integer? A))
        (cons (car D) (cons A (cddr D)))
        #f)))

(define modYear
  (lambda (D A)
    (if (and (date? D) (integer? A))
        (cons (car D) (cons (cadr D) (cons A '())))
        #f)))


(provide (all-defined-out))