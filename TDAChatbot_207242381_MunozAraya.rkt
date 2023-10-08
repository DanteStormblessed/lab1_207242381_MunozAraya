#lang racket

(provide chatbot)
(provide chatbot-add-flow)
(provide chatbot-exists?)
(provide chatbot-code-exists?)
(provide find-chatbot-by-id)
;__________________________________CONSTRUCTOR__________________________________

;Dominio: chatbotID (int) X name (String) X welcomeMessage (String) X startFlowId(int) X  flows*
;Recorrido: chatbot
;Descripcion: Esta funcion construye un chatbot donde se describen sus elementos a traves de una lista
;Tipo de recursion: No aplica

(define (chatbot chatbotID name welcomeMessage startFlowID . flows)
  (list chatbotID name welcomeMessage startFlowID (remove-duplicates flows)))

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
          (if (= (car current-flow) (car flow))
              chatbot
              (add-flow-helper chatbot (cdr flows))))))
  (let ((flows (cdddr chatbot)))
    (if (null? flows)
        (append chatbot (list flow))
        (add-flow-helper chatbot flows))))

;____________________________________________________________________

(define (chatbot-exists? chatbots code)
  (cond
    ((null? chatbots) #f)
    ((= (car chatbots) code) #t)
    (else (chatbot-exists? (cdr chatbots) code))))
;____________________________________________________________________
(define (chatbot-code-exists? chatbots code)
  (cond
    ((null? chatbots) #f)
    ((= (car (car chatbots)) code) #t)
    (else (chatbot-code-exists? (cdr chatbots) code))))
;____________________________________________________________________
(define (find-chatbot-by-id chatbots id)
  (cond
    ((null? chatbots) #f)
    ((= (car chatbots) id) (car chatbots))
    (else (find-chatbot-by-id (cdr chatbots) id))))



