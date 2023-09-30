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

;______________________________________________________________________________________________________

(define (system-login sys user)
  (let* ((name (car sys))
         (initialChatbotCodeLink (cadr sys))
         (chatbots (caddr sys))
         (creationDate (cadddr sys))
         (get-chatHistory (fifth sys))
         (update-chatHistory! (sixth sys))
         (get-users (seventh sys))
         (add-user (eighth sys))
         (add-chatbot (ninth sys))
         (logged-user (tenth sys)))

    (if (member user (get-users))
        (list name initialChatbotCodeLink chatbots creationDate get-chatHistory update-chatHistory! get-users add-user add-chatbot user)
        sys)))

;______________________________________________________________________________________________________

(define (system-logout sys)
  (let* ((name (car sys))
         (initialChatbotCodeLink (cadr sys))
         (chatbots (caddr sys))
         (creationDate (cadddr sys))
         (get-chatHistory (fifth sys))
         (update-chatHistory! (sixth sys))
         (get-users (seventh sys))
         (add-user (eighth sys))
         (add-chatbot (ninth sys))
         (logged-user (tenth sys)))

    (list name initialChatbotCodeLink chatbots creationDate get-chatHistory update-chatHistory! get-users add-user add-chatbot #f)))

;______________________________________________________________________________________________________
(define (system-talk-rec sys message)
  (let* ((name (car sys))
         (initialChatbotCodeLink (cadr sys))
         (chatbots (caddr sys))
         (creationDate (cadddr sys))
         (get-chatHistory (fifth sys))
         (update-chatHistory! (sixth sys))
         (get-users (seventh sys))
         (add-user (eighth sys))
         (add-chatbot (ninth sys))
         (logged-user (tenth sys)))

    (if logged-user
        (let* ((chatbot (list-ref chatbots initialChatbotCodeLink))
               (response (chatbot-talk-rec chatbot message)))
          (let* ((new-chatHistory (update-chatHistory! logged-user message))
                 (new-chatbots (update-chatbots chatbots initialChatbotCodeLink chatbot)))
            (list name initialChatbotCodeLink new-chatbots creationDate new-chatHistory get-users add-user add-chatbot logged-user)))

        sys)))


;______________________________________________________________________________________________________
(define (system-talk-norec sys message)
  (let* ((name (car sys))
         (initialChatbotCodeLink (cadr sys))
         (chatbots (caddr sys))
         (creationDate (cadddr sys))
         (get-chatHistory (fifth sys))
         (update-chatHistory! (sixth sys))
         (get-users (seventh sys))
         (add-user (eighth sys))
         (add-chatbot (ninth sys))
         (logged-user (tenth sys)))

    (if logged-user
        (let* ((chatbot (list-ref chatbots initialChatbotCodeLink))
               (response (chatbot-talk-norec chatbot message)))
          (let* ((new-chatHistory (update-chatHistory! logged-user message))
                 (new-chatbots (update-chatbots chatbots initialChatbotCodeLink chatbot)))
            (list name initialChatbotCodeLink new-chatbots creationDate new-chatHistory get-users add-user add-chatbot logged-user)))

        sys)))

;______________________________________________________________________________________________________

(define (system-synthesis sys user)
  (let* ((name (car sys))
         (initialChatbotCodeLink (cadr sys))
         (chatbots (caddr sys))
         (creationDate (cadddr sys))
         (get-chatHistory (fifth sys))
         (update-chatHistory! (sixth sys))
         (get-users (seventh sys))
         (add-user (eighth sys))
         (add-chatbot (ninth sys))
         (logged-user (tenth sys)))

    (if logged-user
        (let* ((user-chatHistory (get-chatHistory logged-user))
               (chatbot-synthesis (synthesize-chatbot chatbots user-chatHistory))
               (formatted-chatbot-synthesis (format-chatbot-synthesis chatbot-synthesis)))

          formatted-chatbot-synthesis)

        "No hay sesión de usuario iniciada.")))
;______________________________________________________________________________________________________

(define (system-simulate sys maxInteractions seed)
  (let* ((name (car sys))
         (initialChatbotCodeLink (cadr sys))
         (chatbots (caddr sys))
         (creationDate (cadddr sys))
         (get-chatHistory (fifth sys))
         (update-chatHistory! (sixth sys))
         (get-users (seventh sys))
         (add-user (eighth sys))
         (add-chatbot (ninth sys))
         (logged-user (tenth sys)))

    (if logged-user
        (let* ((interaction-history (simulate-dialogue chatbots maxInteractions seed))
               (new-chatHistory (append interaction-history (get-chatHistory logged-user)))
               (new-chatbots (reset-chatbots chatbots)))

          (list name initialChatbotCodeLink new-chatbots creationDate new-chatHistory get-users add-user add-chatbot logged-user))

        sys)))

;______________________________________________________________________________________________________

(define (simulate-dialogue chatbots maxInteractions seed)
  (define (simulate chatbot1 chatbot2 interactions seed)
    (if (or (<= interactions 0) (null? chatbot1) (null? chatbot2))
        '()
        (let* ((message1 (chatbot-talk-norec (car chatbot1) (list-ref chatbot2 (random (length chatbot2))) seed))
               (message2 (chatbot-talk-norec (car chatbot2) (list-ref chatbot1 (random (length chatbot1))) seed)))

          (cons (list message1 'user) (cons (list message2 'user) (simulate chatbot1 chatbot2 (- interactions 2) (myRandom seed)))))))

  (set! seed (myRandom seed)) ; Inicializamos la semilla

  (let* ((numChatbots (length chatbots))
         (chatbot1 (list-ref chatbots (random numChatbots)))
         (chatbot2 (list-ref chatbots (random numChatbots))))

    (simulate chatbot1 chatbot2 maxInteractions seed)))

;______________________________________________________________________________________________________


;______________________________________________________________________________________________________


(define (chatbot-talk-rec chatbot message)
  (if (null? chatbot)
      "No hay opciones disponibles."
      (let* ((option (find-option chatbot message)))
        (if option
            (let* ((nextChatbot (list-ref chatbot (cadr option)))
                   (response (car option)))
              (string-append response "\n" (chatbot-talk-rec nextChatbot message)))

            "No se encontró una opción válida."))))

(define (chatbot-talk-norec chatbot message)
  (if (null? chatbot)
      "No hay opciones disponibles."
      (let* ((option (find-option chatbot message)))
        (if option
            (let* ((nextChatbot (list-ref chatbot (cadr option)))
                   (response (car option)))
              (string-append response "\n" (chatbot-talk-norec nextChatbot message)))

            "No se encontró una opción válida."))))


;______________________________________________________________________________________________________



(define (synthesize-chatbot chatbots user-chatHistory)
  (if (null? chatbots)
      '()
      (let* ((chatbot (car chatbots))
             (chatbotCode (car chatbot))
             (chatbotOptions (cdr chatbot))
             (synthesizedOption (synthesize-option chatbotCode chatbotOptions user-chatHistory))
             (nextChatbots (synthesize-chatbot (cdr chatbots))))

        (if synthesizedOption
            (cons (list synthesizedOption chatbotCode) nextChatbots)
            nextChatbots))))

(define (synthesize-option chatbotCode options user-chatHistory)
  (define (count-appearances keyword history)
    (if (null? history)
        0
        (+ (count-appearances keyword (cdr history))
           (count-appearances keyword (car history)))))


  (let* ((userMessages (filter (lambda (entry) (eq? (caddr entry) 'user)) user-chatHistory))
         (userKeywords (map (lambda (entry) (car (cadr entry))) userMessages))
         (uniqueKeywords (remove-duplicates userKeywords))
         (maxKeyword (apply max (map (lambda (keyword) (count-appearances keyword userKeywords)) uniqueKeywords))))
    
    (if (> maxKeyword 0)
        (let* ((bestOption (find-best-option options uniqueKeywords maxKeyword)))
          (if bestOption
              (cadr bestOption)
              #f))

        #f)))

(define (find-best-option options keywords maxKeyword)
  (define (count-appearances keyword option)
    (if (null? option)
        0
        (+ (count-appearances keyword (cdr option))
           (if (member keyword (cddr (car option))) 1 0))))

  (define (find-best-option-aux options bestOption bestCount)
    (if (null? options)
        bestOption
        (let* ((count (count-appearances (car options) (car options))))
          (if (> count bestCount)
              (find-best-option-aux (cdr options) (car options) count)
              (find-best-option-aux (cdr options) bestOption bestCount)))))


  (find-best-option-aux options (car options) (count-appearances (car options) (car options))))

(define (format-chatbot-synthesis chatbotSynthesis)
  (define (format-chatbot-synthesis-aux chatbotSynthesis formattedSynthesis)
    (if (null? chatbotSynthesis)
        formattedSynthesis
        (format-chatbot-synthesis-aux (cdr chatbotSynthesis)
                                     (string-append formattedSynthesis
                                                    (format "~a: ~a\n"
                                                            (cadr (car chatbotSynthesis))
                                                            (car (car chatbotSynthesis)))))))

  (format-chatbot-synthesis-aux chatbotSynthesis ""))






(define (myRandom Xn)
  (modulo (+ (* 1103515245 Xn) 12345) 2147483648)
)






