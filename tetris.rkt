;;Alunos-Autores: Antonio Gabriel Martins Marcoli, RA: 39866
;;                Gilmar Lima de Souza, RA: 39831
;;Disciplina: 5200 - Paradigma de Programação Lógico e Funcional

#lang racket

;; Você deve implementar as funções neste arquivo. Novas funções podem ser
;; criadas, mas todas as funções devem ter testes (no arquivo testes.rkt).
;;
;; Observe que algumas destas funções não tem testes, faz parte do trabalho
;; criar estes testes.
;;
;; Você não precisa se preocupar com ler a tecla pressionada ou desenhar o jogo
;; na tela. O arquivo main.rkt chama uma função que faz isso. Basta você
;; implementar as funções deste arquivo que o jogo funciona.
;;
;; Para ter uma ideia do processo de execução do jogo, execute o arquivo
;; main.rkt sem mudar nada neste arquivo. Uma janela irá aparecer. Pressione
;; algumas teclas e observe a saída no console do DrRacket. Veja o corpo
;; inicial das funções make-tetris-padrao, trata-tecla, trata-tick e desenha.

(require "base.rkt")
(require 2htdp/image)
(require 2htdp/universe)
;;Biblioteca necessária para converter numero para string (usado para exibir o score na tela)
(require racket/string) 

(provide make-tetris-padrao
         tetramino->lista-pos
         lop-validas?
         elemento-n 
         elemento-xy
         lop-livres?
         fixa
         limpa
         trata-tecla
         trata-tick
         desenha
         nivel)

;; -> Tetris
;; Cria o jogo inicial.
;; Esta função é chamada no arquivo main.rkt.
(define (make-tetris-padrao)
  (make-tetris LARGURA-PADRAO ALTURA-PADRAO (stream-tetraminos) TIMEOUT-PADRAO 0))

;; Tetris KeyEvent -> Tetris
;; Esta função é chamada quando uma tecla é pressionada.
;; Devolve um jogo com o tetraminó que está caindo movido de acordo com a tecla
;;   "right" - tenta mover para direita
;;   "left"  - tenta mover para esquerda
;;   "up"    - tenta rotacionar
;;   "down"  - tenta mover para baixo
;;
;; Se a tecla for "right", "left" ou "up" e o movimento não puder ser
;; realizado, o jogo é devolvido sem modificações.
;;
;; Se a tecla for "down" e o movimento não puder ser realizado, tetra é fixado
;; no campo, as linhas completas são removidas, o próximo tetraminó é
;; selecionada para cair e o contador de automovimento retorna ao valor
;; inicial.
;;
;; Se o movimento puder ser realizado, o jogo após o movimento é devolvido.
;;
;; Use a função key=? para comparar o tecla com os valores "right", "left, "up"
;; e "down".

;;Template Trata-tecla
#;(define (trata-tecla jogo tecla)
    (let ([proximaposicao ...(tetris-tetra jogo)...tecla])
      (cond
        [(key=? tecla "down") (if (lop-livres? (tetramino->lista-pos proximaposicao) (tetris-campo jogo))
                                  ...jogo
                                  jogo)]
        [(key=? tecla " ") proximaposicao...(trata-tecla jogo tecla)...(fixa jogo)]
        [(key=? tecla "right")(if (lop-livres? (tetramino->lista-pos proximaposicao) (tetris-campo jogo))
                                  ...jogo
                                  jogo)]
        [(key=? tecla "left")(if (lop-livres? (tetramino->lista-pos proximaposicao) (tetris-campo jogo))
                                 ...jogo
                                 jogo)]
        [(key=? tecla "up")(if (lop-livres? (tetramino->lista-pos proximaposicao) (tetris-campo jogo))
                               ...jogo
                               jogo)]
        [(key=? tecla "\r")...reiniciar jogo]
        [else jogo])))

(define (trata-tecla jogo tecla)
  
  ;; -> num
  ;; Variável responsável por calcular a velocidade do jogo de acordo com o nível de dificuldade atual
  ;; Para que o jogo fique mais rápido a cada nível, subtraimos do valor do TIMEOUT-PADRAO o valor do nível
  ;; enquanto esse nível for menor que 80% do valor TIMEOUT-PADRAO;
  ;; ex: TIMEOUT-PADRAO = 20; (nivel jogo) = 4
  ;;     TIMEOUT-NIVEL = 16
  (define TIMEOUT-NIVEL 
    (cond
      [(= (nivel jogo) 1) TIMEOUT-PADRAO] ;;se o jogo estiver no nível 1, é usado o TIMEOUT-PADRAO
      [(> (nivel jogo) (* TIMEOUT-PADRAO 0.8)) (* TIMEOUT-PADRAO 0.8)] 
      [else (- TIMEOUT-PADRAO (nivel jogo))]))
  
  ;;Tetramino -> num (rotação)
  ;;Sub-função responsável por determinar a próxima rotação de um tetramino baseado no seu tipo
  (define (rotaciona peca)
    (cond
      [(= (length (tetramino-tipo peca)) 1) 0] ;;Tetramino Quadrado
      [(= (length (tetramino-tipo peca)) 2) (remainder (add1 (tetramino-rot peca)) 2)] ;; Tetramino Linha
      [else (remainder (add1 (tetramino-rot peca)) 4)]))
  
  ;;jogo -> jogo
  ;;Sub-função resposável por finalizar a rodada, fixando a peca, limpando as linhas completas e atualizando o timer do jogo
  ;;Devido ao racket não possuir mudança de estado, utilizamos um iterador para garantir que o objeto jogo seja alterado
  ;;na ordem certa pelas funções fixa e limpa
  (define (finalizacao jogo iter) 
    (cond
      [(zero? iter) (finalizacao (fixa jogo) (add1 iter))]
      [(= 1 iter) (finalizacao (limpa jogo) (add1 iter))]
      ;;Condição necessária quando não existem mais movimentos válidos para a próxima peça (Fim de Jogo)
      [(not (lop-livres? (tetramino->lista-pos (centraliza (stream-first (tetris-proximos jogo)) LARGURA-PADRAO)) 
                         (tetris-campo jogo))) jogo]
      [else (struct-copy tetris jogo 
                         [tetra (centraliza (stream-first (tetris-proximos jogo)) LARGURA-PADRAO)]
                         [proximos (stream-rest (tetris-proximos jogo))]
                         [timeout TIMEOUT-NIVEL])]))
  
  ;;tetramino tecla -> tetramino
  ;;sub-função que recebe um tetramino e uma tecla e devolve o mesmo tetramino com a sua posição
  ;;alterada de acordo com a direção indicada pela tecla
  ;;no caso da tecla UP, é modificada apenas a sua rotação
  (define (mudardirecao tetra tecla)
    (cond
      [(or (key=? tecla "down")
           (key=? tecla " ")) (struct-copy tetramino tetra [pos (posn (add1 (posn-lin (tetramino-pos tetra)))
                                                                      (posn-col (tetramino-pos tetra)))])]
      [(key=? tecla "right") (struct-copy tetramino tetra [pos (posn (posn-lin (tetramino-pos tetra))
                                                                     (add1 (posn-col (tetramino-pos tetra))))])]
      [(key=? tecla "left") (struct-copy tetramino tetra [pos (posn (posn-lin (tetramino-pos tetra))
                                                                    (sub1 (posn-col (tetramino-pos tetra))))])]
      [(key=? tecla "up") (struct-copy tetramino tetra [rot (rotaciona tetra)])]
      [else tetra]))
  
  (let ([proximaposicao (mudardirecao (tetris-tetra jogo) tecla)])
    (cond
      [(or (key=? tecla "down")
           (key=? tecla " ")) (cond 
                                [(lop-livres? (tetramino->lista-pos proximaposicao) (tetris-campo jogo)) 
                                 (if (key=? tecla "down")
                                     (struct-copy tetris jogo 
                                                  [tetra proximaposicao] 
                                                  [timeout TIMEOUT-NIVEL])
                                     ;;Caso a tecla = espaço (" ")
                                     (trata-tecla (struct-copy tetris jogo 
                                                               [tetra proximaposicao]) " "))]
                                ;;se não houver mais movimentos posiveis para baixo, a rodada deve ser finalizada
                                [else (finalizacao jogo 0)])]
      [(or (key=? tecla "right")
           (key=? tecla "left")
           (key=? tecla "up")) (if (lop-livres? (tetramino->lista-pos proximaposicao) (tetris-campo jogo))
                                   (struct-copy tetris jogo 
                                                [tetra proximaposicao])
                                   jogo)]
      ;;Tratamento para a tecla ENTER
      ;;Só será considerada quando o jogo está finalizado (sem movimentos possíveis para o próxima peça no stream)
      ;;sua função é reiniciar o jogo
      [(key=? tecla "\r") (if (not (lop-livres? (tetramino->lista-pos (centraliza (stream-first (tetris-proximos jogo)) LARGURA-PADRAO)) 
                                                (tetris-campo jogo))) 
                              (struct-copy tetris jogo 
                                           [campo (make-campo LARGURA-PADRAO ALTURA-PADRAO)]
                                           [tetra (centraliza (stream-first (tetris-proximos jogo)) LARGURA-PADRAO)]
                                           [proximos (stream-rest (tetris-proximos jogo))]
                                           [timeout TIMEOUT-PADRAO]
                                           [pontos 0])
                              jogo)]
      [else jogo])))

;; Tetris -> Tetris
;; Função que trata um tick. Esta função é chamada 28 vezes por segundo, ela
;; deve mover o tetra para baixo depois de uma determinada quantidade
;; (TIMEOUT-PADRAO) de ticks. Se o jogador mover para baixo e fixar o
;; tetraminó, a contagem deve reiniciar.

;;Template trata-tick
#;(define (trata-tick jogo)
    (if (zero? proximo tick)
        ...movimento o tetramino atual para baixo
        ..devolve o jogo com o tick menor))

(define (trata-tick jogo)
  
  ;;variável local que armazena um cópia do jogo com o próximo timeout
  (define jogotick 
    (struct-copy tetris jogo 
                 [timeout (sub1 (tetris-timeout jogo))]))
  
  (if (zero? (tetris-timeout jogotick))
      (trata-tecla jogotick "down")
      jogotick))

;; Tetris -> Imagem
;; Esta função é chamada quando o jogo precisa ser desenhado na tela. Devolve
;; uma imagem que representa o jogo.
;; Veja as funções pré-definidas rectangle, beside, above e overlay (entre
;; outras)no pacote 2htdp/image.
(define (desenha jogo)
  
  ;;inteiro boolean -> imagem
  ;;sub-função que recebe uma cor e um valor booleano e devolve o desenho de um quadrado Q-LARGURA x Q-ALTURA  
  ;;preenchido com essa cor e com uma borda preta
  ;;caso o valor booleano seja verdadeiro, será devolvido um quadrado transparente
  (define (quadrado cor transparente)
    (if (not transparente)
        (underlay (rectangle Q-LARGURA Q-ALTURA 200 cor)
                  (rectangle (/ Q-LARGURA 1.5) (/ Q-ALTURA 1.5) "solid" cor)
                  (rectangle Q-LARGURA Q-ALTURA "outline" "black"))
        (rectangle Q-ALTURA Q-LARGURA 0 cor)))
  
  ;;lista num -> imagem
  ;;sub-função que recebe uma lista e uma cor e desenha na tela um quadrado para cada 
  ;;elemento da lista
  (define (desenhalinha lst cor)
    (cond
      [(empty? lst) BLANK]
      [(and (not (zero? cor)) (= (first lst) 1)) (beside (quadrado (elemento-n CORES cor) #f) 
                                                         (desenhalinha (rest lst) cor))] ;;condição necessária para colorir as peças
      [(and (not (zero? cor)) (zero? (first lst))) (beside (quadrado (elemento-n CORES cor) #t) ;;quadrado transparente 
                                                           (desenhalinha (rest lst) cor))]
      [else (beside (quadrado (elemento-n CORES (first lst)) #f) (desenhalinha (rest lst) cor))]))
  
  ;;lista num -> imagem
  ;;sub-função que recebe um campo e uma cor e imprime todo o campo na tela
  (define (desenhacampo campo cor)
    (cond
      [(empty? campo) BLANK]
      [else (above (desenhalinha (first campo) cor) (desenhacampo (rest campo) cor))]))
  
  
  ;;Construção do painel lateral que conterá a previsão da próxima peça, nível e score do jogo atual
  (define painellateral 
    (underlay/align "center" "top"
                    (rectangle (* LARGURA-PADRAO (/ Q-LARGURA 1.5)) (* ALTURA-PADRAO Q-ALTURA) "outline" "black")
                    (above (text/font "Próximo" 16 "black" "Gill Sans" 'swiss 'normal 'bold #f)
                           (text/font "Tetraminó" 16 "black" "Gill Sans" 'swiss 'normal 'bold #f)
                           (overlay/align "center" "middle"
                                          (desenhacampo (elemento-n (tetramino-tipo (stream-first (tetris-proximos jogo))) 
                                                                    (tetramino-rot (stream-first (tetris-proximos jogo))))
                                                        (tetramino-cor (stream-first (tetris-proximos jogo))))
                                          (rectangle 85 85 20 "seagreen")
                                          (rectangle 90 90 "solid" "silver")
                                          (rectangle 100 100 "solid" "seagreen")
                                          (rectangle 120 120 "solid" "white"))
                           (beside (text/font "Nível: " 16 "black" "Gill Sans" 'swiss 'normal 'bold #f)
                                   (text/font (~r (nivel jogo)) 16 "black" "Gill Sans" 'swiss 'normal 'bold #f))
                           (beside (text/font "Pontos: " 16 "black" "Gill Sans" 'swiss 'normal 'bold #f)
                                   (text/font (~r (tetris-pontos jogo)) 16 "black" "Gill Sans" 'swiss 'normal 'bold #f)))))
  
  (beside (if (not (lop-livres? (tetramino->lista-pos (centraliza (stream-first (tetris-proximos jogo)) LARGURA-PADRAO)) 
                                (tetris-campo jogo)))
              (place-image/align (above (text/font "GAME OVER" 25 "yellow" "Gill Sans" 'swiss 'normal 'bold #f)
                                        (text/font "Pressione Enter" 20 "white" "Gill Sans" 'swiss 'normal 'bold #f)
                                        (text/font "para reiniciar" 20 "white" "Gill Sans" 'swiss 'normal 'bold #f))
                                 (+ LARGURA-PADRAO 90)
                                 (+ ALTURA-PADRAO 180)
                                 "center" "center"
                                 (desenhacampo (tetris-campo jogo) 0))
              (place-image/align (desenhacampo (elemento-n (tetramino-tipo (tetris-tetra jogo)) (tetramino-rot (tetris-tetra jogo))) 
                                               (tetramino-cor (tetris-tetra jogo))) 
                                 (* Q-LARGURA (posn-col (tetramino-pos (tetris-tetra jogo))))
                                 (* Q-ALTURA (posn-lin (tetramino-pos (tetris-tetra jogo))))
                                 "left" "top"
                                 (desenhacampo (tetris-campo jogo) 0)))
          painellateral))

;; Tetramino -> Lista(Posn)
;; Devolve a lista de posições que t ocupa no campo considerando a rotação e a
;; posição (translação em relação a origem).
;; 
;; Por exemplo, seja TT1 definido como
;; (define TT1 (tetramino T_TIPOS 1 (posn 1 0) T_COR))
;; este tetraminó está na rotação 1 e na posição (posn 1 0). O elemento na
;; posição 1 de T_TIPOS é T1 que é a seguinte lista de listas (definina em
;; tetra-tipos.rkt)
;;    0 1 2     ; colunas
;;              ; linhas
;; '((0 1 0)    ; 0
;;   (0 1 1)    ; 1
;;   (0 1 0)))  ; 2
;;
;; As as posições ocupadas por T1 são marcadas com 1, ou seja, as posições
;; ocupadas por T1 são (posn 0 1) (posn 1 1) (posn 1 2) e (posn 2 1). Estas São
;; as posições em relação a (posn 0 0), mas o tetraminó está na posição
;; (posn 1 0), desta forma, precisamos fazer a translação das posições. Para
;; isto, somamos o ponto (posn 1 0) a cada ponto de T1, o que resulta em
;; (pos 1 1) (posn 2 1) (posn 2 2) (posn 3 1). Observe que é posível ter
;; um deslocamento em relação a origem negativa. Por exemplo, se a posição de
;; TT1 fosse (posn 0 -1), obteríamos como resposta da função a lista com
;; as posições (posn 0 0) (posn 1 0) (pos 1 1) (pos 2 0).
;;
;; Veja os testes para outros exemplos de como esta função deve funcionar.

;;Template da função tetramino->lista-pos
#;(define (tetramino->lista-pos t)
    (define max (length ((tetramino-tipo t) ... (tetramino-rot t))))
    (define (devolve-PosValidas lst lin col)
      (cond
        [(>= lin max) empty]
        [(>= col max) (devolve-PosValidas lst ..lin..col)]
        [(= (...lst ...lin ..col) 1) (cons (posn ...lin ...col (+ lin (posn-lin (tetramino-pos t)))
                                                 (devolve-PosValidas lst ...lin ...col)))]
        [else (devolve-PosValidas lst ...lin ...col)]))
    (devolve-PosValidas ...((tetramino-tipo t) (tetramino-rot t)) 0 0))

;;Corpo da Função tetramino->lista-pos
(define (tetramino->lista-pos t)
  
  (define max (length (elemento-n (tetramino-tipo t) (tetramino-rot t)))) ;;Pegando proporção da matriz (ex max = 3 -> Matriz 3 x 3)
  
  (define (devolve-PosValidas lst lin col)
    (cond
      [(>= lin max) empty] ;; Percorreu todas as linhas da matriz
      [(>= col max) (devolve-PosValidas lst (add1 lin) 0)] ;; Percorreu toda a linha atual e passa para a próxima linha
      [(not (zero? (elemento-xy lst lin col))) (cons (posn (+ lin (posn-lin (tetramino-pos t)))
                                                           (+ col (posn-col (tetramino-pos t))))
                                                     (devolve-PosValidas lst lin (add1 col)))] ;; Devolve a posição preenchida pela peça
      [else (devolve-PosValidas lst lin (add1 col))]))
  
  (devolve-PosValidas (elemento-n (tetramino-tipo t) (tetramino-rot t)) 0 0)) ;;Inicia a varredura da matriz da peça pela posição 0 0 

;; Lista(Posn) Natural Natural -> Boolean
;; Devolve verdadeiro se todas os posições de lp são válidas, isto é, estão
;; dentro de um campo de tamanho largura x altura. Devolve falso caso
;; contrário.

;;Template da Função lop-validas?
#;(define (lop-validas? lp largura altura) 
    (cond 
      [(empty? lp) #t]                                        ; passou por todas as posições com sucesso
      [(...(posn-lin (first lp))...(posn-col (first lp))) #f] ; valida a posn com a largura e altura
      [else (lop-validas? (rest lp) largura altura)]))        ; se passou vai para a próxima posição

;;Corpo da Função lop-validas?
(define (lop-validas? lp largura altura) 
  (cond 
    [(empty? lp) #t] 
    [(nand (and (>= (posn-lin (first lp)) 0) (< (posn-lin (first lp)) altura))
           (and (>= (posn-col (first lp)) 0) (< (posn-col (first lp)) largura))) #f]  
    [else (lop-validas? (rest lp) largura altura)]))

;;==================================
;; Função auxiliar elemento-n
;; lista n --> elemento
;; Devolve o n-ésimo elemento contido em lista
#;(define (elemento-n lst n) empty)

;;Template elemento-n
#;(define (elemento-n lst n)
    (cond 
      [(empty? lst) empty]
      [(= n 0) (first lst)]
      [else (elemento-n (rest lst) ...n)]))

;;Corpo função elemento-n
(define (elemento-n lst n)
  (cond 
    [(empty? lst) empty]
    [(zero? n) (first lst)]
    [else (elemento-n (rest lst) (sub1 n))]))

;; Função auxiliar elemento-xy
;; lista x y --> elemento
;; Devolve o elemento na coordenada x y de uma matriz
#;(define (elemento-xy lst x y) empty)

;;Template elemento-xy
#;(define (elemento-xy lst x y)
    (cond 
      [(empty? lst) empty]
      [else ... lst 
            ... x
            ... y]))

;;Corpo função elemento-xy
(define (elemento-xy lst x y)
  (cond 
    [(empty? lst) empty]
    [else (elemento-n (elemento-n lst x) y)]))

;; Lista(Posn) Campo -> Boolean
;; Devolve verdadeiro se todas as posição de lp estão livres no campo. Devolve
;; falso caso contrário.
;; Requer que todas as posições em lp sejam válidas.

;;Template da Função lop-livres?
#;(define (lop-livres? lp campo)
    (cond 
      [(empty? lp) #t]
      [(...(posn-lin (first lp))...(posn-col (first lp)) ...campo) #f]
      [else (lop-livres? (rest lp) campo)]))

;;Corpo da Função Lop-livres?
(define (lop-livres? lp campo)
  (if (lop-validas? lp (length (first campo)) (length campo))
      (cond 
        [(empty? lp) #t]
        [(not (zero? (elemento-xy campo 
                                  (posn-lin (first lp)) 
                                  (posn-col (first lp))))) #f] ;;acessando a posição xy da matriz campo
        [else (lop-livres? (rest lp) campo)])
      #f))

;; Tetris -> Tetris
;; Preenche as posições ocupadas pelo tetraminó (que está caindo) no campo do
;; jogo.
;; Requer que tetraminó não possa ser movido para baixo.

;;Template Fixa
#;(define (fixa jogo)    
    (let ([pos_tetramino (tetramino->lista-pos (tetris-tetra jogo))]
          [cor_tetramino (tetramino-cor (tetris-tetra jogo))])
      (if (not (lop-livres? pos_tetramino (tetris-campo jogo))) 
          (struct-copy tetris jogo 
                       [campo (preenche (tetris-campo jogo)   ;;Caso o tetraminó não possa ser movido para baixo
                                        pos_tetramino
                                        cor_tetramino)])
          jogo)))

;;Corpo da função Fixa
(define (fixa jogo)
  
  ;;campo inteiro inteiro elemento -> campo
  ;;preenche a posição x y de um campo com elemento determinado
  (define (alteracampo campo linha coluna elem)
    
    (define (alteralinha lst pos)
      (if (zero? pos) 
          (cons elem (rest lst))
          (cons (first lst) (alteralinha (rest lst) (sub1 pos)))))
    
    (cond 
      [(zero? linha) (cons (alteralinha (first campo) coluna) (rest campo))]
      [else (cons (first campo) (alteracampo (rest campo) (sub1 linha) coluna elem))]))
  
  
  ;;campo lista-pos cor -> campo
  ;;sub-função responsável por preencher o campo com a cor informada baseada na lista de posições
  (define (preenche campo lst-pos cor)
    (cond 
      [(empty? lst-pos) campo]
      [else (preenche (alteracampo campo (posn-lin (first lst-pos)) (posn-col (first lst-pos)) cor)
                      (rest lst-pos)
                      cor)]))
  
  (let ([pos_tetramino (tetramino->lista-pos (tetris-tetra jogo))]
        [cor_tetramino (tetramino-cor (tetris-tetra jogo))]
        ;;Criando uma copia do tetramino movido para baixo
        [tetramino_baixo (struct-copy tetramino (tetris-tetra jogo) 
                                      [pos (posn (add1 (posn-lin (tetramino-pos (tetris-tetra jogo))))
                                                 (posn-col (tetramino-pos (tetris-tetra jogo))))])])
    (if (not (lop-livres? (tetramino->lista-pos tetramino_baixo) (tetris-campo jogo))) ;;Caso o tetraminó não possa ser movido para baixo
        (struct-copy tetris jogo 
                     [campo (preenche (tetris-campo jogo)   
                                      pos_tetramino
                                      cor_tetramino)])
        jogo)))

;; Tetris -> Tetris
;; Devolve um jogo sem as linhas que estão completas, isto é, as linhas que não
;; tem nenhum quadrado vazio. O jogo devolvido tem o mesmo tamanho do jogo de
;; entrada.

(define (limpa jogo)
  
  ;; campo -> campo
  ;; Devolve um novo campo sem as linhas que já estão preenchidas  
  (define (remove-linha campo)
    (cond 
      [(empty? campo) empty]
      [(empty? (filter zero? (first campo))) (remove-linha (rest campo))]       ;;linha preenchida
      [else (cons (first campo) (remove-linha (rest campo)))]))
  
  ;;campo natural -> campo
  ;;preenche um campo com n linhas vazias
  (define (preenchecampo campo linhas)
    (cond 
      [(zero? linhas) campo]
      [else (preenchecampo (cons (make-linha (tetris-largura jogo)) campo) (sub1 linhas))]))
  
  (let ([campolimpo (remove-linha (tetris-campo jogo))])
    (struct-copy tetris jogo 
                 [campo (preenchecampo campolimpo 
                                       (- (tetris-altura jogo) (length campolimpo)))]
                 [pontos (+ (tetris-pontos jogo) (* PONTOS-LINHA (- (tetris-altura jogo) (length campolimpo))))])))

;; -> Stream(Tetramino)
;; Cria um stream randômico de tetraminós.
;; Esta função não precisa de testes.
;; Você tem que implementar esta função, o corpo incial deve ser descartado.
;;Extra: Criamos o random para a rotação da peça

(define (stream-tetraminos)
  (define (rot-max peca)
    (cond [(equal? 1 (tetramino-cor peca)) 2]
          [(equal? 4 (tetramino-cor peca)) 1]
          [else 4]))
  (define peca (elemento-n TETRAMINOS (random 7)))
  (stream-cons (struct-copy tetramino peca 
                            [rot (random (rot-max peca))]) (stream-tetraminos)))

;;jogo -> natural
;;função que calcula em qual nível de velocidade está o jogo atual
(define (nivel jogo) 
  (add1 (quotient (tetris-pontos jogo) PONTOS-NIVEL)))