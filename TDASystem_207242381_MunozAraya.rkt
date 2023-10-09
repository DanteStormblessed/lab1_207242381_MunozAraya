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


;________________________________________________________________________________________________________

;Dominio: name X initialChatbotCodeLink X . chatbots
;Recorrido: System
;Descripcion: Crea un nuevo sistema de chatbots con el nombre name, un 
;enlace de código inicial initialChatbotCodeLink y una lista opcional de chatbots chatbots.
;Tipo de recursion: No aplica
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
    (list name initialChatbotCodeLink chatbot-list chatbot-codes '() '() '())))


;________________________________________________________________________________________________________

;Dominio: System X chatbot
;Recorrido: System
;Descripcion: Añade un chatbot al sistema, primero verifica si el chatbot existe.
;Tipo de recursion: No aplica
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

;________________________________________________________________________________________________________
;Dominio: System X user
;Recorrido: System
;Descripcion: añade un usuario al sistema comprobando si existe
;Tipo de recursion: No aplica
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


;________________________________________________________________________________________________________
;Dominio: System X user
;Recorrido: System
;Descripcion: Verifica si existe el usuario, para añadir un parámetro al sistema con el usuario logeado
;Tipo de recursion: No aplica
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
              user))));aqui se añade el usuario al system como un nuevo parametro
;________________________________________________________________________________________________________
;Dominio: System
;Recorrido: System
;Descripcion: Cierra la sesión del usuario en el sistema, quita el parámetro añadido a 
;system, devolviendo un #f en ese caso
;Tipo de recursion: No aplica
(define (system-logout system)
  (list (first system)
        (second system)
        (third system)
        (fourth system)
        (fifth system)
        #f))
;________________________________________________________________________________________________________

;Dominio: System X message
;Recorrido: System
;Descripcion: Permite interactuar con el sistema, toma en cuenta el flujo inicial, además 
;de un mensaje inicial dado por el usuario, trabaja de forma recursiva gracias a la función Chathistory
;Tipo de recursion: la recursion se hace en update-chatHistory
(define (system-talk-rec system message)
  (let ((logged-user (sixth system)))
    (if logged-user
        (let ((chatbot (find-chatbot-by-id (third system) (second system))))
          (if chatbot
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
        system)))



;________________________________________________________________________________________________________

;Dominio: System X message
;Recorrido: System
;Descripcion: Permite interactuar con el sistema, toma en cuenta el flujo inicial, además 
;de un mensaje inicial dado por el usuario
;Tipo de recursion: no aplica
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
;________________________________________________________________________________________________________

;Dominio: System X user
;Recorrido: list
;Descripcion: ofrece una síntesis del sistema, formeateado a una lista de strings para 
;poder ser visualizado con Display
;Tipo de recursion: no aplica
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
;________________________________________________________________________________________________________
;Dominio: system X maxInteractions X seed
;Recorrido: list
;Descripcion: Función para poder simular un dialogo entre dos chatbos, usa una semilla 
;que se genera con la función myrandom, además de un número máximo de interacciones
;Tipo de recursion: no aplica
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
;________________________________________________________________________________________________________

;Dominio: chatHistory X user X message
;Recorrido: list
;Descripcion: función que permite actualizar el historial del chat, se llama cada vez 
;que se añade un mensaje en el sistema, permite que no se pierdan los mensajes anteriores
;Tipo de recursion: no aplica
(define (update-chatHistory chatHistory user message)
  (let* ((timestamp (current-inexact-milliseconds))
         (formatted-timestamp (format "~a" timestamp))
         (entry (list formatted-timestamp message 'system)))
    (append chatHistory (list (list user entry)))))

;________________________________________________________________________________________________________

;Dominio: system maxInteractions seed
;Recorrido: list
;Descripcion: función que toma un historial de chat y lo reformatea para 
;presentarlo de manera diferente
;Tipo de recursion: no aplica
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

;________________________________________________________________________________________________________

;Dominio: system X maxInteractions X seed
;Recorrido: system X maxInteractions X seed
;Descripcion: Esta función simula un diálogo entre dos chatbots en el sistema. Utiliza 
;un máximo de interacciones especificado y una semilla para la generación de números 
;aleatorios. La simulación de diálogo entre chatbots implica que los chatbots intercambien 
;mensajes, elijan opciones y naveguen a través de flujos predefinidos
;Tipo de recursion: no aplica
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
;________________________________________________________________________________________________________
(define (myRandom Xn)
  (modulo (+ (* 1103515245 Xn) 12345) 2147483648)
)

