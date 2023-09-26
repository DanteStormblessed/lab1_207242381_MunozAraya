#lang racket

(provide chatbot)
(provide chatbot-add-flow)
;__________________________________CONSTRUCTOR__________________________________

;Dominio: chatbotID (int) X name (String) X welcomeMessage (String) X startFlowId(int) X  flows*
;Recorrido: chatbot
;Descripcion: Esta funcion construye un chatbot donde se describen sus elementos a traves de una lista
;Tipo de recursion: No aplica

(define (chatbot chatbotID name welcomeMessage startFlowID . flows)
  (list chatbotID name welcomeMessage startFlowID flows))
;__________________________________MODIFICADOR__________________________________

;Dominio: chatbot X flow
;Recorrido: chatbot
;Descripcion: Se toma como argumentos un chatbot y un flujo. lo añade a la lista de flujos por medio de recursion de cola.
;Tipo de recursion: Recursion de cola

(define (chatbot-add-flow chatbot flow)
  (define (add-flow-helper chatbot flows)
    (if (null? flows)
        (append chatbot (list flow))
        (let ((current-flow (car flows)))
          (if (= (cadr current-flow) (cadr flow))
              chatbot
              (add-flow-helper chatbot (cdr flows))))))
  (let ((flows (cddr chatbot)))
    (if (null? flows)
        (append chatbot (list flow))
        (add-flow-helper chatbot flows))))


