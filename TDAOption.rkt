#lang racket


(provide option)


;La funcion "option" tiene como finalidad la construccion de opciones para el flujo de un Chatbot
;__________________________________CONSTRUCTOR__________________________________

;Dominio: code (Int)  X message (String)  X ChatbotCodeLink (Int) X FlowCodeLink (Int) X Keyword
;Recorrido: Option
;Descripcion: Crea una lista que contiene los elementos asociados al dominio de la misma funcion
;Tipo de recursion: No aplica

(define (option code message chatbot-code initial-flow-code . keywords)
  (list code message chatbot-code initial-flow-code keywords))
;_______________________________________________________________________________
