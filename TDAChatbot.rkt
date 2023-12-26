#lang racket

(provide chatbot)
(provide chatbot-add-flow)
(provide chatbot-exists?)
(provide find-chatbot-by-id)
;__________________________________CONSTRUCTOR___________________________________________________________

;Dominio: chatbotID (int) X name (String) X welcomeMessage (String) X startFlowId(int) X  flows*
;Recorrido: chatbot
;Descripcion: Esta funcion construye un chatbot donde se describen sus elementos a traves de una lista
;Tipo de recursion: No aplica

(define (chatbot chatbotID name welcomeMessage startFlowID . flows)
  (list chatbotID name welcomeMessage startFlowID (remove-duplicates flows)))
;__________________________________MODIFICADOR___________________________________________________________

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
;________________________________________________________________________________________________________

;Dominio: chatbot X code
;Recorrido: Bool
;Descripcion: verifica si un chatbot con un código específico existe en la lista de chatbots proporcionada como argumento.
;Tipo de recursion: Recursion de cola
(define (chatbot-exists? chatbots code)
  (cond
    ((null? chatbots) #f)
    ((= (car (car chatbots)) code) #t)
    (else (chatbot-exists? (cdr chatbots) code))))
;________________________________________________________________________________________________________

;Dominio: chatbot X code
;Recorrido: chatbot X bool
;Descripcion:  itera recursivamente sobre la lista de chatbots y compara la identificación (ID) del chatbot actual con la ID proporcionada.
;Si encuentra un chatbot con la misma ID, devuelve ese chatbot. Si no encuentra ninguna coincidencia, devuelve #f.
;Tipo de recursion: Recursion de cola
(define (find-chatbot-by-id chatbots id)
  (cond
    ((null? chatbots) #f)
    ((= (car (car chatbots)) id) (car chatbots))
    (else (find-chatbot-by-id (cdr chatbots) id))))




