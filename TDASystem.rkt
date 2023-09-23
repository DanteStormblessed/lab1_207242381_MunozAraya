#lang racket



;__________________________________CONSTRUCTOR__________________________________

;Dominio: name (string) X InitialChatbotCodeLink (Int) X chatbot*
;Recorrido: system
;Descripcion: Construye un sistema donde se deja registro del chat y de la fecha de creacion
;Tipo de recursion: No aplica

(define (system name initialChatbotCodeLink . chatbots)
  (let* ((creationDate (current-inexact-milliseconds))
         (chatHistory (make-hash)))
    (define (get-chatHistory user)
      (hash-ref chatHistory user '()))

    (define (update-chatHistory! user message)
      (let* ((timestamp (current-inexact-milliseconds))
             (formatted-timestamp (format "~a" timestamp))
             (entry (list formatted-timestamp message 'system)))
        (let ((current-history (get-chatHistory user)))
          (hash-set! chatHistory user (cons entry current-history)))))

    (list name initialChatbotCodeLink chatbots creationDate get-chatHistory update-chatHistory!)))




