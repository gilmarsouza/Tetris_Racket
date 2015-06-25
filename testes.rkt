#lang racket

(require rackunit)
(require rackunit/text-ui)
(require "tetra-tipos.rkt")
(require "base.rkt")
(require "tetris.rkt")

;; Constantes usadas nos testes
(define TIMEOUT 14)
(define PONTOS 0)

(define TT1 (tetramino T_TIPOS 1 (posn 1 0) T_COR))
(define TT1-RIGHT (tetramino T_TIPOS 1 (posn 1 1) T_COR))
(define TT1-LEFT (tetramino T_TIPOS 1 (posn 1 -1) T_COR))
(define TT1-UP (tetramino T_TIPOS 2 (posn 1 0) T_COR))
(define TT1-DOWN (tetramino T_TIPOS 1 (posn 2 0) T_COR))

(define TT1_POS (list (posn 1 1)
                      (posn 2 1) (posn 2 2)
                      (posn 3 1)))
(define TT1_CENTRA_10 (tetramino T_TIPOS 1 (posn 1 3) T_COR))

(define TZ2 (tetramino Z_TIPOS 2 (posn 2 3) Z_COR))
(define TZ2_POS (list (posn 3 3) (posn 3 4)
                      (posn 4 4) (posn 4 5)))
(define TZ2_CENTRA_15 (tetramino Z_TIPOS 2 (posn 2 6) Z_COR))

(define TI0 (tetramino I_TIPOS 0 (posn -1 1) I_COR))
(define TI0_POS (list (posn 0 1) (posn 0 2) (posn 0 3) (posn 0 4)))
(define TI0_CENTRA_12 (tetramino I_TIPOS 0 (posn -1 4) I_COR))

(define C1 (list (list 0 0 0 0 0 0 0)   ; 0
                 (list 0 0 0 0 0 0 0)   ; 1
                 (list 6 0 0 0 0 0 0)   ; 2
                 (list 4 0 2 4 6 1 1)   ; 3
                 (list 3 4 0 0 0 0 0)   ; 4
                 (list 1 2 4 3 2 5 6))) ; 5
;     0 1 2 3 4 5 6

(define C1_LARGURA 7)
(define C1_ALTURA 6)
;; algumas posições ocupadas em C1
(define C1_OCUPADAS (list (posn 2 0) (posn 3 2) (posn 4 1)))
;; algumas posições livres em C1
(define C1_LIVRES (list (posn 0 0) (posn 3 1) (posn 4 2)))

(define C T_COR)

; Representa C1 com o tetraminó TT1 fixado no campo
(define C1_FIXA_TT1 (list (list 0 0 0 0 0 0 0)   ; 0
                          (list 0 C 0 0 0 0 0)   ; 1
                          (list 6 C C 0 0 0 0)   ; 2
                          (list 4 C 2 4 6 1 1)   ; 3
                          (list 3 4 0 0 0 0 0)   ; 4
                          (list 1 2 4 3 2 5 6))) ; 5
;     0 1 2 3 4 5 6

; Representa C1_FIXA_TT1 sem as linha completas
(define C1_FIXA_TT1_LIMPA (list (list 0 0 0 0 0 0 0)   ; 0
                                (list 0 0 0 0 0 0 0)   ; 1
                                (list 0 0 0 0 0 0 0)   ; 2
                                (list 0 C 0 0 0 0 0)   ; 3
                                (list 6 C C 0 0 0 0)   ; 4
                                (list 3 4 0 0 0 0 0))) ; 5
;     0 1 2 3 4 5 6

(define C2 (list (list 0 0 0 0 0)
                 (list 0 0 0 0 0)
                 (list 0 0 0 0 0)
                 (list 0 0 0 0 0)
                 (list 0 0 0 0 0)
                 (list 0 0 0 0 0)
                 (list 0 0 0 0 0)))

(define C2_LARGURA 5)
(define C2_ALTURA 7)

(define CAMPOTESTE1 (list (list 0 0 0 0 0 0 0 0 0 0)   ;0
                          (list 0 0 0 0 0 0 0 0 0 0)   ;1
                          (list 0 0 0 0 0 0 0 0 0 0)   ;2
                          (list 0 0 0 0 0 0 0 0 0 0)   ;3
                          (list 0 0 0 0 0 0 0 0 0 0)   ;4
                          (list 0 0 0 0 0 0 0 0 0 0)   ;5
                          (list 0 0 0 0 0 0 0 0 0 0)   ;6
                          (list 0 0 0 0 0 0 0 0 0 0)   ;7
                          (list 0 0 0 0 0 0 0 0 0 0)   ;8
                          (list 0 0 0 0 0 0 0 0 0 0)   ;9
                          (list 0 0 0 0 0 0 0 0 0 0)   ;10
                          (list 0 0 0 0 0 0 0 0 0 0)   ;11
                          (list 0 0 0 0 0 0 0 0 0 0)   ;12
                          (list 0 0 0 0 0 0 0 0 0 0)   ;13
                          (list 0 0 0 0 0 0 0 0 0 0)   ;14
                          (list 0 0 0 0 0 0 0 0 0 0)   ;15
                          (list 0 0 0 0 0 0 0 0 0 0)   ;16
                          (list 0 0 0 0 0 0 0 0 0 0)   ;17
                          (list 0 0 0 0 0 0 0 0 0 0)   ;18
                          (list 0 0 0 0 0 0 0 0 0 0))) ;19
;;                              0 1 2 3 4 5 6 7 8 9

(define CAMPOTESTE1-T1FIXADO (list (list 0 0 0 0 0 0 0 0 0 0)   ;0
                                   (list 0 0 0 0 0 0 0 0 0 0)   ;1
                                   (list 0 0 0 0 0 0 0 0 0 0)   ;2
                                   (list 0 0 0 0 0 0 0 0 0 0)   ;3
                                   (list 0 0 0 0 0 0 0 0 0 0)   ;4
                                   (list 0 0 0 0 0 0 0 0 0 0)   ;5
                                   (list 0 0 0 0 0 0 0 0 0 0)   ;6
                                   (list 0 0 0 0 0 0 0 0 0 0)   ;7
                                   (list 0 0 0 0 0 0 0 0 0 0)   ;8
                                   (list 0 0 0 0 0 0 0 0 0 0)   ;9
                                   (list 0 0 0 0 0 0 0 0 0 0)   ;10
                                   (list 0 0 0 0 0 0 0 0 0 0)   ;11
                                   (list 0 0 0 0 0 0 0 0 0 0)   ;12
                                   (list 0 0 0 0 0 0 0 0 0 0)   ;13
                                   (list 0 0 0 0 0 0 0 0 0 0)   ;14
                                   (list 0 0 0 0 0 0 0 0 0 0)   ;15
                                   (list 0 0 0 0 0 0 0 0 0 0)   ;16
                                   (list 0 C 0 0 0 0 0 0 0 0)   ;17
                                   (list 0 C C 0 0 0 0 0 0 0)   ;18
                                   (list 0 C 0 0 0 0 0 0 0 0))) ;19
;;                                       0 1 2 3 4 5 6 7 8 9

(define CAMPOTESTE1-GAMEOVER (list (list C C C C C C C C C 0)   ;0
                                   (list C C C C C C C 0 C 0)   ;1
                                   (list C C C C C C C 0 0 0)   ;2
                                   (list C C C C C C 0 0 0 0)   ;3
                                   (list 0 C C C C C 0 0 0 0)   ;4
                                   (list 0 0 0 C C C 0 0 0 0)   ;5
                                   (list 0 0 0 0 C C 0 0 0 0)   ;6
                                   (list 0 0 0 0 C 0 0 0 0 0)   ;7
                                   (list 0 0 0 0 C 0 0 0 0 0)   ;8
                                   (list 0 0 0 0 C 0 0 0 0 0)   ;9
                                   (list 0 0 0 0 C 0 0 0 0 0)   ;10
                                   (list 0 0 0 0 C 0 0 0 0 0)   ;11
                                   (list 0 0 0 0 C 0 0 0 0 0)   ;12
                                   (list 0 0 0 0 C 0 0 0 0 0)   ;13
                                   (list 0 0 0 0 C 0 0 0 0 0)   ;14
                                   (list 0 0 0 0 C 0 0 0 0 0)   ;15
                                   (list 0 0 0 0 C 0 0 0 0 0)   ;16
                                   (list 0 C 0 0 C 0 0 0 0 0)   ;17
                                   (list 0 C C 0 C 0 0 0 0 0)   ;18
                                   (list 0 C 0 0 C 0 0 0 0 0))) ;19
;;                                       0 1 2 3 4 5 6 7 8 9



(define JOGOTESTE
  (tetris CAMPOTESTE1
          LARGURA-PADRAO
          ALTURA-PADRAO
          TT1
          (list TZ2 TI0)
          TIMEOUT-PADRAO
          0))

(define JOGOTESTE-RIGHT
  (struct-copy tetris JOGOTESTE [tetra TT1-RIGHT]))

(define JOGOTESTE-LEFT
  (struct-copy tetris JOGOTESTE [tetra TT1-LEFT]))

(define JOGOTESTE-DOWN
  (struct-copy tetris JOGOTESTE [tetra TT1-DOWN]))

(define JOGOTESTE-UP
  (struct-copy tetris JOGOTESTE [tetra TT1-UP]))

(define JOGOTESTE-SPACE
  (struct-copy tetris JOGOTESTE
               [campo CAMPOTESTE1-T1FIXADO]
               [tetra (centraliza TZ2 LARGURA-PADRAO)]
               [proximos (rest (tetris-proximos JOGOTESTE))]))

(define JOGOTESTE-GAMEOVER
  (struct-copy tetris JOGOTESTE
               [campo CAMPOTESTE1-GAMEOVER]))

(define JOGOTESTE-ENTER
  (struct-copy tetris JOGOTESTE-GAMEOVER
               [campo CAMPOTESTE1]
               [tetra (first (tetris-proximos JOGOTESTE-GAMEOVER))]
               [proximos (rest (tetris-proximos JOGOTESTE-GAMEOVER))]))

(define JOGOTESTE-MENOS1TICK
  (struct-copy tetris JOGOTESTE 
               [timeout (sub1 (tetris-timeout JOGOTESTE))]))

(define JOGOTESTE-ZEROTICK
  (struct-copy tetris JOGOTESTE 
               [timeout 1]))

(define JOGOTESTE-NIVEL
  (struct-copy tetris JOGOTESTE 
               [pontos 650]))


(define trata-tecla-tests
  (test-suite
   "trata-tecla tests"
   (check-equal? (trata-tecla JOGOTESTE "right") JOGOTESTE-RIGHT)
   (check-equal? (trata-tecla JOGOTESTE "left") JOGOTESTE-LEFT)
   (check-equal? (trata-tecla JOGOTESTE "down") JOGOTESTE-DOWN)
   (check-equal? (trata-tecla JOGOTESTE "up") JOGOTESTE-UP)
   (check-equal? (trata-tecla JOGOTESTE " ") JOGOTESTE-SPACE)
   (check-equal? (trata-tecla JOGOTESTE-GAMEOVER "\r") JOGOTESTE-ENTER)
   (check-equal? (trata-tecla JOGOTESTE "rshift") JOGOTESTE)))

(define trata-tick-tests
  (test-suite
   "trata-tick tests"
   (check-equal? (trata-tick JOGOTESTE) JOGOTESTE-MENOS1TICK)
   (check-equal? (trata-tick JOGOTESTE-ZEROTICK) JOGOTESTE-DOWN)))

(define make-linha-tests
  (test-suite
   "make-linha tests"
   (check-equal? (make-linha 0) empty)
   (check-equal? (make-linha 5) (list 0 0 0 0 0))))

(define make-campo-tests
  (test-suite
   "make-campo tests"
   (check-equal? (make-campo C2_LARGURA C2_ALTURA) C2)))

(define centraliza-tests
  (test-suite
   "centraliza tests"
   (check-equal? (centraliza TT1 10)
                 TT1_CENTRA_10)
   (check-equal? (centraliza TZ2 15)
                 TZ2_CENTRA_15)
   (check-equal? (centraliza TI0 12)
                 TI0_CENTRA_12)))

(define make-tetris-tests
  (test-suite
   "make-tetris tests"
   (check-equal? (make-tetris C2_LARGURA C2_ALTURA (list TT1 TZ2 TI0) TIMEOUT PONTOS)
                 (tetris C2
                         C2_LARGURA
                         C2_ALTURA
                         (centraliza TT1 C2_LARGURA)
                         (list TZ2 TI0)
                         TIMEOUT
                         0))))

(define tetramino->pos-tests
  (test-suite
   "tetramino->pos tests"
   (check-equal? (tetramino->lista-pos TT1) TT1_POS)
   (check-equal? (tetramino->lista-pos TZ2) TZ2_POS)
   (check-equal? (tetramino->lista-pos TI0) TI0_POS)))

(define lop-validas?-tests
  (test-suite
   "lop-validas? tests"
   (check-equal? (lop-validas? empty 5 8)
                 #t)
   ;; testa os extremos
   (check-equal? (lop-validas? (list (posn 0 0)  
                                     (posn (sub1 C1_ALTURA) 0)  
                                     (posn 0 (sub1 C1_LARGURA))   
                                     (posn (sub1 C1_ALTURA) (sub1 C1_LARGURA))) 
                               C1_LARGURA
                               C1_ALTURA) 
                 #t)
   (check-equal? (lop-validas? (list (posn 0 0)
                                     (posn C1_ALTURA 0) ; linha inválida
                                     (posn 1 2))
                               C1_LARGURA
                               C1_ALTURA)
                 #f)
   (check-equal? (lop-validas? (list (posn  2 3)
                                     (posn -1 3)) ; linha inválida
                               C1_LARGURA
                               C1_ALTURA)
                 #f)
   (check-equal? (lop-validas? (list (posn 0 0)
                                     (posn 0 C1_LARGURA) ; coluna inválida
                                     (posn 1 2))
                               C1_LARGURA
                               C1_ALTURA)
                 #f)
   (check-equal? (lop-validas? (list (posn 0 0)
                                     (posn 1 -1)) ; coluna inválida
                               C1_LARGURA
                               C1_ALTURA)
                 #f)))

(define lop-livres?-tests
  (test-suite
   "lop-livres? tests"
   (check-equal? (lop-livres? C1_LIVRES C1) #t)
   (check-equal? (lop-livres? C1_OCUPADAS C1) #f)
   (check-equal? (lop-livres? (append C1_LIVRES (list (first C1_OCUPADAS))) C1) #f)))

(define fixa-tests
  (test-suite
   "fixa tests"
   (check-equal? (fixa (tetris C1 C1_LARGURA C1_ALTURA TT1 empty TIMEOUT PONTOS))
                 (tetris C1_FIXA_TT1 C1_LARGURA C1_ALTURA TT1 empty TIMEOUT PONTOS))))

(define limpa-tests
  (test-suite
   "limpa tests"
   (check-equal? (limpa (tetris C1_FIXA_TT1 C1_LARGURA C1_ALTURA TT1 empty TIMEOUT PONTOS))
                 (tetris C1_FIXA_TT1_LIMPA C1_LARGURA C1_ALTURA TT1 empty TIMEOUT (* PONTOS-LINHA 2)))))

;;Testes extras-------------------
(define elemento-n-tests
  (test-suite
   "elemento-n tests"
   (check-equal? (elemento-n (list (list 1 2 3) (list 4 5 6)) 2) empty)
   (check-equal? (elemento-n (list (list 1 2 3 6) (list 4 5 6 9) (list 8 9 3 1)) 1) (list 4 5 6 9))
   (check-equal? (elemento-n (list (list 1 2 3 6) (list 4 5 6 9) (list 8 9 3 1)) 0) (list 1 2 3 6))
   (check-equal? (elemento-n (list 1 2 3 6) 0) 1)
   (check-equal? (elemento-n (list 4 9 7 1) 2) 7)
   (check-equal? (elemento-n (list 5 9 7 1 59 32) 6) empty)))

(define elemento-xy-tests
  (test-suite
   "elemento-xy tests"
   (check-equal? (elemento-xy C1 10 20) empty)
   (check-equal? (elemento-xy C1 0 2) 0)
   (check-equal? (elemento-xy C1 2 0) 6)
   (check-equal? (elemento-xy C1 5 4) 2)))

(define nivel-tests
  (test-suite
   "nivel tests"
   (check-equal? (nivel JOGOTESTE) 1)
   (check-equal? (nivel JOGOTESTE-NIVEL) 7)))

;; ---------------------------------------------------------------------
;; Função que executa um grupo de testes.
(define (executar-testes . testes)
  (run-tests (test-suite "Todos os testes" testes))
  (void))

;; Chama a função para executar os testes.
(executar-testes make-linha-tests
                 make-campo-tests
                 centraliza-tests
                 make-tetris-tests
                 tetramino->pos-tests
                 lop-validas?-tests
                 lop-livres?-tests
                 fixa-tests
                 limpa-tests
                 elemento-n-tests
                 elemento-xy-tests
                 trata-tecla-tests
                 trata-tick-tests
                 nivel-tests)