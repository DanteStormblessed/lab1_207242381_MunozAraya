#lang racket


(provide flow)
(provide flow-add-option)
(provide find-flow-by-id)
;__________________________________CONSTRUCTOR__________________________________

;Dominio: id (int) X name (String)  X Option*
;Recorrido: flow
;Descripcion: Esta funcion construye el flujo a partir de una id 
;Tipo de recursion: No aplica

(define (flow id name . options)
  (list id name options))

;__________________________________MODIFICADOR__________________________________

;Dominio: flow X option
;Recorrido: flow
;Descripcion: Esta funcion modifica el flow para poder añadir opciones
;Tipo de recursion: No aplica

(define (flow-add-option flow option)
  (let ((options (cddr flow)))
    (if (not (member option options)) 
        (cons (car flow) (cadr flow) (cons option options)) 
        flow)))


(define (find-flow-by-id flows id)
  (cond
    ((null? flows) #f)
    ((= (caar flows) id) (car flows))
    (else (find-flow-by-id (cdr flows) id))))
