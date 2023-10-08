#lang racket


(provide flow)
(provide flow-add-option)
(provide find-flow-by-id)
(provide remove-duplicates)
;__________________________________CONSTRUCTOR__________________________________

;Dominio: id (int) X name (String)  X Option*
;Recorrido: flow
;Descripcion: Esta funcion construye el flujo a partir de una id 
;Tipo de recursion: No aplica

(define (flow id name . options)
  (list id name (remove-duplicates options)))
;__________________________________MODIFICADOR__________________________________

;Dominio: flow X option
;Recorrido: flow
;Descripcion: Esta funcion modifica el flow para poder añadir opciones
;Tipo de recursion: No aplica

(define (flow-add-option flow option)
  (let* ((id (car option))
         (new-options (remove-duplicates (cons option (caddr flow)))))
    (list (car flow) (cadr flow) new-options)))


(define (remove-flow-by-id flows id)
  (filter (lambda (f) (not (= (car f) id))) flows))

(define (remove-duplicates lst)
  (if (null? lst)
      '()
      (cons (car lst)
            (remove-duplicates (filter (lambda (item) (not (equal? (car item) (car (car lst))))) (cdr lst))))))



(define (find-flow-by-id flows id)
  (cond
    ((null? flows) #f)
    ((= (caar flows) id) (car flows))
    (else (find-flow-by-id (cdr flows) id))))
;____________________________________________________________________


