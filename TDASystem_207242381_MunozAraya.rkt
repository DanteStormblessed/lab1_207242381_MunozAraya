#lang racket
(require "TDAChatbot_207242381_MunozAraya.rkt")
(require "TDAFlow_207242381_MunozAraya.rkt")

(provide system)
(provide system-add-chatbot)
(provide system-add-user)
(provide system-login)
(provide system-logout)
(provide system-talk-rec)
(provide system-talk-norec)
(provide system-synthesis)
(provide system-simulate)
(provide update-chatHistory)
(provide synthesize-chatHistory)


(define (myRandom Xn)
  (modulo (+ (* 1103515245 Xn) 12345) 2147483648)
)

;; TDA system - constructor
(define (system name initialChatbotCodeLink . chatbots)
  (define (add-chatbots chatbots chatbot-list chatbot-codes)
    (if (null? chatbots)
        (list chatbot-list chatbot-codes)
        (let* ((chatbot (car chatbots))
               (id (car chatbot)))
          (if (member id chatbot-codes)
              (add-chatbots (cdr chatbots) chatbot-list chatbot-codes)
              (add-chatbots (cdr chatbots)
                            (cons chatbot chatbot-list)
                            (cons id chatbot-codes))))))
  (let* ((chatbot-list-and-codes (add-chatbots chatbots '() '()))
         (chatbot-list (car chatbot-list-and-codes))
         (chatbot-codes (cadr chatbot-list-and-codes)))
    (list name initialChatbotCodeLink chatbot-list chatbot-codes '() #f)))


;; TDA system - modificador para añadir chatbots a un sistema
(define (system-add-chatbot system chatbot)
  (define (chatbot-exists? chatbot-codes chatbotID)
    (if (null? chatbot-codes)
        #f
        (if (equal? (car chatbot-codes) chatbotID)
            #t
            (chatbot-exists? (cdr chatbot-codes) chatbotID))))
  (let ((chatbot-list (third system))
        (chatbot-codes (fourth system)))
    (if (chatbot-exists? chatbot-codes (car chatbot))
        system
        (list (first system)
              (second system)
              (cons chatbot chatbot-list)
              (cons (car chatbot) chatbot-codes)
              (fifth system)
              (sixth system)))))


;; TDA system - modificador para añadir usuarios a un sistema
(define (system-add-user system user)
  (define (user-exists? users user)
    (if (null? users)
        #f
        (if (equal? (car users) user)
            #t
            (user-exists? (cdr users) user))))
  (let ((users (fifth system)))
    (if (user-exists? users user)
        system
        (list (first system)
              (second system)
              (third system)
              (fourth system)
              (cons user users)
              (sixth system)))))



;; TDA system - función que permite iniciar una sesión en el sistema
(define (system-login system user)
  (define (user-exists? users user)
    (if (null? users)
        #f
        (if (equal? (car users) user)
            #t
            (user-exists? (cdr users) user))))
  (let ((users (fifth system))
        (logged-user (sixth system)))
    (if (or logged-user (not (user-exists? users user)))
        system
        (list (first system)
              (second system)
              (third system)
              (fourth system)
              (fifth system)
              user))))

;; TDA system - función que permite cerrar una sesión abierta
(define (system-logout system)
  (list (first system)
        (second system)
        (third system)
        (fourth system)
        (fifth system)
        #f))


;; system-talk-rec - Función que permite interactuar con un chatbot (usando recursividad)
(define (system-talk-rec system message)
  (if (sixth system)
      (let ((logged-user (sixth system)))
        (if (find-chatbot-by-id (third system) logged-user)
            (let* ((chatHistory (seventh system))
                   (new-history (update-chatHistory chatHistory logged-user message)))
              (list (first system)
                    (second system)
                    (third system)
                    (fourth system)
                    (fifth system)
                    logged-user
                    new-history))
            system))
      system))



;; system-talk-norec - Función que permite interactuar con un chatbot (implementación declarativa)
(define (system-talk-norec system message)
  (if (sixth system)
      (let ((logged-user (sixth system)))
        (if (chatbot-exists? (fourth system) logged-user)
            (let* ((chatHistory (seventh system))
                   (new-history (update-chatHistory chatHistory logged-user message)))
              (list (first system)
                    (second system)
                    (third system)
                    (fourth system)
                    (fifth system)
                    (sixth system)
                    new-history))
            system)
          system)
      system)
)

;; system-synthesis - Función que ofrece una síntesis del chatbot para un usuario particular
(define (system-synthesis system user)
  (if (sixth system)
      (let* ((logged-user (sixth system))
             (chatHistory (seventh system)))
        (if (equal? logged-user user)
            (synthesize-chatHistory chatHistory)
            '())
          '())
      '())
)

;; system-simulate - Función para simular un diálogo entre dos chatbots
(define (system-simulate system maxInteractions seed)
  (if (sixth system)
      (let* ((logged-user (sixth system))
             (chatHistory (seventh system)))
        (if logged-user
            (simulate-dialogue system maxInteractions seed)
            '())
          '())
      '())
)
;____________________________________________________________________________________
(define (update-chatHistory chatHistory user message)
  (let* ((timestamp (current-inexact-milliseconds))
         (formatted-timestamp (format "~a" timestamp))
         (entry (list formatted-timestamp message 'system)))
    (hash-set! chatHistory user (cons entry (hash-ref chatHistory user '())))))
;____________________________________________________________________________________
(define (synthesize-chatHistory chatHistory)
  (let loop ((entries (hash->list chatHistory))
             (result '()))
    (cond
      ((null? entries) (reverse result))
      (else
       (let* ((entry (car entries))
              (timestamp (car entry))
              (message (cadr entry))
              (sender (caddr entry)))
         (loop (cdr entries)
               (cons (list timestamp message sender) result)))))))



;____________________________________________________________________________________
(define (simulate-dialogue system maxInteractions seed)
  (let* ((logged-user (sixth system))
         (chatHistory (seventh system))
         (chatbot-list (third system))
         (user-chatbot (find-chatbot-by-id chatbot-list logged-user)))
    (if user-chatbot
        (let loop ((interactions 0)
                   (current-chatbot user-chatbot)
                   (current-history (hash-ref chatHistory logged-user '()))
                   (random-seed seed)
                   (result '()))
          (if (>= interactions maxInteractions)
              (reverse result)
              (let* ((current-flow (find-flow-by-id (cddr current-chatbot) (cadr current-chatbot)))
                     (options (cddr current-flow))
                     (random-index (myRandom random-seed))
                     (chosen-option (list-ref options random-index)))
                (let* ((formatted-timestamp (format "~a" (current-inexact-milliseconds)))
                       (entry (list formatted-timestamp chosen-option 'system)))
                  (let ((new-history (cons entry current-history)))
                    (loop (+ interactions 1)
                          (find-chatbot-by-id chatbot-list (cadr chosen-option))
                          new-history
                          (myRandom random-index)
                          (cons chosen-option result)))))))
        '())
    )
)


