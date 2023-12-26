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
;________________________________________________________________________________________________________

;Dominio: flow X id
;Recorrido: flow
;Descripcion: función itera recursivamente sobre la lista de flujos y elimina el flujo que tiene la misma identificación (ID) que la proporcionada.
;Retorna una nueva lista de flujos sin el flujo eliminado.
;Tipo de recursion: No aplica
(define (remove-flow-by-id flows id)
  (filter (lambda (f) (not (= (car f) id))) flows))
;________________________________________________________________________________________________________

;Dominio: list
;Recorrido: list
;Descripcion: función itera recursivamente sobre una lista para eliminar los duplicados, utiliza la función filter
;Tipo de recursion: Natural
(define (remove-duplicates lst)
  (if (null? lst)
      '()
      (cons (car lst)
            (remove-duplicates (filter (lambda (item) (not (equal? (car item) (car (car lst))))) (cdr lst))))))
;________________________________________________________________________________________________________

;Dominio: Flow X id
;Recorrido: Bool X Flow
;Descripcion: Esta función itera sobre la lista de flujos y compara la identificación (ID) del 
;flujo actual con la ID proporcionada
;Tipo de recursion: Natural
(define (find-flow-by-id flows id)
  (cond
    ((null? flows) #f)
    ((= (caar flows) id) (car flows))
    (else (find-flow-by-id (cdr flows) id))))
;________________________________________________________________________________________________________

