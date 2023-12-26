#lang racket
(require "TDAOption_207242381_MunozAraya.rkt")
(require "TDAFlow_207242381_MunozAraya.rkt")
(require "TDASystem_207242381_MunozAraya.rkt")
(require "TDAChatbot_207242381_MunozAraya.rkt")


;scripts de prueba 3

(define op1 (option  1 "1) ver peliculas" 1 1 "peliculas" "pelis"))
(define op2 (option  2 "2) buscar videojuegos" 2 1 "jugar" "videojuegos" "juegos"))
(define f1 (flow 1 "Flujo Principal Chatbot 1\nBienvenid@\n¿Qué deseas realizar?" op1 op2))
(define f2 (flow-add-option f1 op1))           
(define cb0 (chatbot 0 "Inicial" "Bienvenid@\n¿Que deseas realizar?" 1 f1))
(define op3 (option 1 "1) Terror" 1 2 "miedo" "horror" "tension"))
(define op4 (option 2 "2) ciencia ficcion" 1 1 "sci-fi" "ficcion"))
(define op5 (option 3 "3) fantasia" 1 1 "magia" "medieval"))
(define op6 (option 1 "1) 2023" 1 3 "23")) 
(define op7 (option 2 "2) 2022" 1 3 "22"))
(define op8 (option 3 "3) 2021" 1 3 "21"))
(define op9 (option 4 "4) otros" 1 2 "Volver" "mas"))
(define op10 (option 5 "5) otro genero" 1 1 "generos"))
(define f3 (flow 1 "Flujo 1 Chatbot1\n¿Que genero quieres ver?" op3 op4 op5))
(define f4 (flow 3 "Flujo 3 Chatbot1\n¿De que año?" op6 op7 op8 op9 op10))
(define cb1 (chatbot 1 "Bot peliculas"  " Bienvenido\n¿Que pelicula quieres ver?" 1 f3 f4))
(define op11 (option 1 "1) Pc" 2 1 "computador"))
(define op12 (option 2 "2) Playstation" 2 1 "play" "ps"))
(define op13 (option 3 "3) nintendo switch" 0 1 "nintendo" "switch"))
(define f5 (flow 1 "Flujo 1 Chatbot2\n¿En qué te gustaría jugar?" op11 op12 op13))
(define cb2 (chatbot 2 "Bot Videojuegos"  "Bienvenido\n¿Qué te gustaria jugar?" 1 f5))
(define s0 (system "Chatbots entretenimiento" 0 cb0 cb1 cb2))
(define s1 (system-add-chatbot s0 cb0))
(define s2 (system-add-user s1 "usuario1"))
(define s3 (system-login s2 "usuario2"))