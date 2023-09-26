#lang racket

(provide system)
(provide system-add-chatbot)
(provide system-add-user)
;__________________________________CONSTRUCTOR__________________________________

;Dominio: name (string) X InitialChatbotCodeLink (Int) X chatbot*
;Recorrido: system
;Descripcion: Construye un sistema donde se deja registro del chat y de la fecha de creacion
;Tipo de recursion: No aplica

(define (system name initialChatbotCodeLink . chatbots)
  (let* ((creationDate (current-inexact-milliseconds))
         (chatHistory '())
         (users '()))

    (define (get-chatHistory user)
      (cond
        ((equal? (car chatHistory) user) (cdr chatHistory))
        (else (cons user '()))))

    (define (update-chatHistory user message)
      (let* ((timestamp (current-inexact-milliseconds))
             (formatted-timestamp (format "~a" timestamp))
             (entry (list formatted-timestamp message 'user)))

        (cond
          ((equal? (car chatHistory) user)
           (cons user (cons entry (cdr chatHistory))))

          (else (cons user (list entry))))))

    (define (get-users)
      users)

    (define (add-user user)
      (if (not (member user users))
          (cons user users)
          users))

    (define (add-chatbot chatbot)
      (cons chatbot chatbots))

    (list name initialChatbotCodeLink chatbots creationDate get-chatHistory update-chatHistory get-users add-user add-chatbot)))

;______________________________________________________________________________________________________

(define (system-add-chatbot sys chatbot)
  (let* ((name (car sys))
         (initialChatbotCodeLink (cadr sys))
         (chatbots (caddr sys))
         (creationDate (cadddr sys))
         (get-chatHistory (fifth sys))
         (update-chatHistory! (sixth sys))
         (get-users (seventh sys))
         (add-user (eighth sys))
         (add-chatbot (ninth sys)))

    (if (not (member chatbot chatbots))
        (list name initialChatbotCodeLink (cons chatbot chatbots) creationDate get-chatHistory update-chatHistory! get-users add-user add-chatbot)
        sys)))

;______________________________________________________________________________________________________

(define (system-add-user sys user)
  (let* ((name (car sys))
         (initialChatbotCodeLink (cadr sys))
         (chatbots (caddr sys))
         (creationDate (cadddr sys))
         (get-chatHistory (fifth sys))
         (update-chatHistory! (sixth sys))
         (get-users (seventh sys))
         (add-user (eighth sys))
         (add-chatbot (ninth sys)))

    (if (not (member user (get-users)))
        (list name initialChatbotCodeLink chatbots creationDate get-chatHistory update-chatHistory! (cons user (get-users)) add-user add-chatbot)
        sys)))







