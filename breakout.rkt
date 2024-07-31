; Konstanten
(define WIDTH 800) ; Breite des Spielfelds
(define HEIGHT 500) ; Höhe des Spielfelds
(define BALL-IMG (circle 10 "solid" "red")) ; Bild des Balls
(define BALL-RADIUS (/ (image-width BALL-IMG) 2)) ; Radius des Balls

(define BAR-WIDTH 160) ; Breite der Bar
(define BAR-HEIGHT 15) ; Höhe der Bar
(define BAR-IMG (rectangle BAR-WIDTH BAR-HEIGHT "solid" "yellow")) ; Bild der Bar

(define BRICK-WIDTH 100) ; Breite eines Ziegelsteins
(define BRICK-HEIGHT 50) ; Höhe eines Ziegelsteins
(define BRICK-IMG (rectangle BRICK-WIDTH BRICK-HEIGHT "solid" "black")) ; Bild eines Ziegelsteins

; Datenstrukturen
(define-struct vel (delta-x delta-y))
; eine Vel ist eine Struktur: (make-vel Number Number)
; interpretiert den Geschwindigkeitsvektor eines sich bewegenden Objekts

(define-struct ball (loc velocity))
; ein Ball ist eine Struktur: (make-ball Posn Vel)
; interpretiert die Position und Geschwindigkeit eines Objekts

(define-struct world (ball bar-x game-over? started? bricks))
; eine Welt ist eine Struktur: (make-world Ball Number Boolean Boolean (Listof Boolean))
; interpretiert den Zustand der Spielwelt, einschließlich des Balls, der x-Position der Bar, ob das Spiel vorbei ist, ob das Spiel gestartet wurde und die Sichtbarkeit der Ziegelsteine

; Posn Vel -> Posn
; wendet q auf p an und simuliert die Bewegung in einem Zeittakt
(check-expect (posn+vel (make-posn 5 6) (make-vel 1 2))
              (make-posn 6 8))
(define (posn+vel p q)
  (make-posn (+ (posn-x p) (vel-delta-x q))
             (+ (posn-y p) (vel-delta-y q))))

; Ball -> Ball
; berechnet die Bewegung des Balls in einem Zeittakt
(check-expect (move-ball (make-ball (make-posn 20 30)
                                    (make-vel 5 10)))
              (make-ball (make-posn 25 40)
                         (make-vel 5 10)))
(define (move-ball ball)
  (make-ball (posn+vel (ball-loc ball)
                       (ball-velocity ball))
             (ball-velocity ball)))

; A Collision is either
; - "top"
; - "down"
; - "left"
; - "right"
; - "bar"
; - "brick1"
; - "brick2"
; - "brick3"
; - "brick4"
; - "brick5"
; - "brick6"
; - "brick7"
; - "brick8"
; - "none"
; interpretiert den Ort, an dem ein Ball mit einer Wand, der Bar oder einem Ziegelstein kollidiert

; World Posn -> Collision
; erkennt, mit welcher der Wände, der Bar oder den Ziegelsteinen der Ball kollidiert
(check-expect (collision (make-world (make-ball (make-posn 0 12) (make-vel 1 2)) (/ WIDTH 2) #f #f (list #t #t #t #t #t #t #t #t)) (make-posn 0 12)) "left")
(check-expect (collision (make-world (make-ball (make-posn 15 HEIGHT) (make-vel 1 2)) (/ WIDTH 2) #f #f (list #t #t #t #t #t #t #t #t)) (make-posn 15 HEIGHT)) "down")
(check-expect (collision (make-world (make-ball (make-posn WIDTH 12) (make-vel 1 2)) (/ WIDTH 2) #f #f (list #t #t #t #t #t #t #t #t)) (make-posn WIDTH 12)) "right")
(check-expect (collision (make-world (make-ball (make-posn 15 0) (make-vel 1 2)) (/ WIDTH 2) #f #f (list #t #t #t #t #t #t #t #t)) (make-posn 15 0)) "top")
(check-expect (collision (make-world (make-ball (make-posn 55 55) (make-vel 1 2)) (/ WIDTH 2) #f #f (list #t #t #t #t #t #t #t #t)) (make-posn 55 55)) "none")
(define (collision world posn)
  (cond
    [(<= (posn-x posn) BALL-RADIUS) "left"]
    [(<= (posn-y posn) BALL-RADIUS) "top"]
    [(>= (posn-x posn) (- WIDTH BALL-RADIUS)) "right"]
    [(>= (posn-y posn) (- HEIGHT BALL-RADIUS)) "down"]
    [(and (<= (abs (- (world-bar-x world) (posn-x posn))) (/ BAR-WIDTH 2))
          (<= (abs (- (- HEIGHT 50) (posn-y posn))) (/ BAR-HEIGHT 2))) "bar"]
    [(and (list-ref (world-bricks world) 0)
          (<= (abs (- (+ (/ WIDTH 2) (* -1.5 BRICK-WIDTH)) (posn-x posn))) (/ BRICK-WIDTH 2))
          (<= (abs (- 50 (posn-y posn))) (/ BRICK-HEIGHT 2))) "brick1"]
    [(and (list-ref (world-bricks world) 1)
          (<= (abs (- (+ (/ WIDTH 2) (* -0.5 BRICK-WIDTH)) (posn-x posn))) (/ BRICK-WIDTH 2))
          (<= (abs (- 50 (posn-y posn))) (/ BRICK-HEIGHT 2))) "brick2"]
    [(and (list-ref (world-bricks world) 2)
          (<= (abs (- (+ (/ WIDTH 2) (* 0.5 BRICK-WIDTH)) (posn-x posn))) (/ BRICK-WIDTH 2))
          (<= (abs (- 50 (posn-y posn))) (/ BRICK-HEIGHT 2))) "brick3"]
    [(and (list-ref (world-bricks world) 3)
          (<= (abs (- (+ (/ WIDTH 2) (* 1.5 BRICK-WIDTH)) (posn-x posn))) (/ BRICK-WIDTH 2))
          (<= (abs (- 50 (posn-y posn))) (/ BRICK-HEIGHT 2))) "brick4"]
    [(and (list-ref (world-bricks world) 4)
          (<= (abs (- (+ (/ WIDTH 2) (* -1.5 BRICK-WIDTH)) (posn-x posn))) (/ BRICK-WIDTH 2))
          (<= (abs (- 120 (posn-y posn))) (/ BRICK-HEIGHT 2))) "brick5"]
    [(and (list-ref (world-bricks world) 5)
          (<= (abs (- (+ (/ WIDTH 2) (* -0.5 BRICK-WIDTH)) (posn-x posn))) (/ BRICK-WIDTH 2))
          (<= (abs (- 120 (posn-y posn))) (/ BRICK-HEIGHT 2))) "brick6"]
    [(and (list-ref (world-bricks world) 6)
          (<= (abs (- (+ (/ WIDTH 2) (* 0.5 BRICK-WIDTH)) (posn-x posn))) (/ BRICK-WIDTH 2))
          (<= (abs (- 120 (posn-y posn))) (/ BRICK-HEIGHT 2))) "brick7"]
    [(and (list-ref (world-bricks world) 7)
          (<= (abs (- (+ (/ WIDTH 2) (* 1.5 BRICK-WIDTH)) (posn-x posn))) (/ BRICK-WIDTH 2))
          (<= (abs (- 120 (posn-y posn))) (/ BRICK-HEIGHT 2))) "brick8"]
    [else "none"]))

; Vel Collision -> Vel
; berechnet die Geschwindigkeit eines Objekts nach einer Kollision
(check-expect (bounce (make-vel 3 4) "left") (make-vel -3 4))
(check-expect (bounce (make-vel 3 4) "top") (make-vel 3 -4))
(check-expect (bounce (make-vel 3 4) "none") (make-vel 3 4))
(check-expect (bounce (make-vel 3 4) "bar") (make-vel 3 -4))
(check-expect (bounce (make-vel 3 4) "brick1") (make-vel 3 -4))
(check-expect (bounce (make-vel 3 4) "brick2") (make-vel 3 -4))
(check-expect (bounce (make-vel 3 4) "brick3") (make-vel 3 -4))
(check-expect (bounce (make-vel 3 4) "brick4") (make-vel 3 -4))
(check-expect (bounce (make-vel 3 4) "brick5") (make-vel 3 -4))
(check-expect (bounce (make-vel 3 4) "brick6") (make-vel 3 -4))
(check-expect (bounce (make-vel 3 4) "brick7") (make-vel 3 -4))
(check-expect (bounce (make-vel 3 4) "brick8") (make-vel 3 -4))

(define (bounce vel collision)
  (cond [(or (string=? collision "left")
             (string=? collision "right"))
         (make-vel (- (vel-delta-x vel))
                   (vel-delta-y vel))]
        [(or (string=? collision "down")
             (string=? collision "top")
             (string=? collision "bar")
             (string=? collision "brick1")
             (string=? collision "brick2")
             (string=? collision "brick3")
             (string=? collision "brick4")
             (string=? collision "brick5")
             (string=? collision "brick6")
             (string=? collision "brick7")
             (string=? collision "brick8"))
         (make-vel (vel-delta-x vel)
                   (- (vel-delta-y vel)))]
        [else vel]))

; WorldState -> Image
; rendert den Ball an seiner Position
(define (render world)
  (cond
    [(world-game-over? world)
     (if (all-bricks-destroyed? (world-bricks world))
         (place-image (text "You Win!" 50 "green")
                      (/ WIDTH 2) (/ HEIGHT 2)
                      (empty-scene WIDTH HEIGHT))
         (place-image (text "Game Over" 50 "red")
                      (/ WIDTH 2) (/ HEIGHT 2)
                      (empty-scene WIDTH HEIGHT)))]
    [(not (world-started? world))
     (place-image (text "Press any key to start" 30 "green")
                  (/ WIDTH 2) (/ HEIGHT 2)
                  (empty-scene WIDTH HEIGHT))]
    [else
     (render-bricks
      (world-bricks world)
      (place-image BALL-IMG
                   (posn-x (ball-loc (world-ball world)))
                   (posn-y (ball-loc (world-ball world)))
                   (place-image BAR-IMG
                                (world-bar-x world)
                                (- HEIGHT 50)
                                (empty-scene WIDTH HEIGHT))))]))

; rendert die Ziegelsteine
(define (render-bricks bricks scene)
  (if (empty? bricks)
      scene
      (render-bricks-helper bricks 0 scene)))

; hilft beim Rendern der Ziegelsteine
(define (render-bricks-helper bricks index scene)
  (cond
    [(empty? bricks) scene]
    [(first bricks)
     (place-image BRICK-IMG
                  (+ (/ WIDTH 2) (* (- (modulo index 4) 1.5) BRICK-WIDTH))
                  (+ 50 (* (quotient index 4) 70))
                  (render-bricks-helper (rest bricks) (+ index 1) scene))]
    [else
     (render-bricks-helper (rest bricks) (+ index 1) scene)]))

; WorldState -> WorldState
; bewegt den Ball zu seiner nächsten Position
(check-expect (tick (make-world (make-ball (make-posn 20 12) (make-vel 1 2))
                                (/ WIDTH 2) #f #t (list #t #t #t #t #t #t #t #t)))
              (make-world (make-ball (make-posn 21 14) (make-vel 1 2))
                          (/ WIDTH 2) #f #t (list #t #t #t #t #t #t #t #t)))
(define (tick world)
  (if (or (world-game-over? world) (not (world-started? world)))
      world
      (tick-helper (posn+vel (ball-loc (world-ball world))
                             (ball-velocity (world-ball world)))
                   (collision world (posn+vel (ball-loc (world-ball world))
                                              (ball-velocity (world-ball world))))
                   world)))

; hilft beim Tick-Prozess
(define (tick-helper next-pos collision-type world)
  (cond
    [(string=? collision-type "down")
     (make-world (world-ball world) (world-bar-x world) #t #t (world-bricks world))]
    [(or (string=? collision-type "brick1")
         (string=? collision-type "brick2")
         (string=? collision-type "brick3")
         (string=? collision-type "brick4")
         (string=? collision-type "brick5")
         (string=? collision-type "brick6")
         (string=? collision-type "brick7")
         (string=? collision-type "brick8"))
     (make-world
      (make-ball next-pos
                 (bounce (ball-velocity (world-ball world)) collision-type))
      (world-bar-x world)
      (all-bricks-destroyed? (update-bricks collision-type (world-bricks world)))
      #t
      (update-bricks collision-type (world-bricks world)))]
    [else
     (make-world
      (make-ball next-pos
                 (bounce (ball-velocity (world-ball world)) collision-type))
      (world-bar-x world)
      #f
      #t
      (world-bricks world))]))

; aktualisiert den Zustand der Ziegelsteine nach einer Kollision
(define (update-bricks collision-type bricks)
  (cond
    [(string=? collision-type "brick1") (list #f (second bricks) (third bricks) (fourth bricks) (fifth bricks) (sixth bricks) (seventh bricks) (eighth bricks))]
    [(string=? collision-type "brick2") (list (first bricks) #f (third bricks) (fourth bricks) (fifth bricks) (sixth bricks) (seventh bricks) (eighth bricks))]
    [(string=? collision-type "brick3") (list (first bricks) (second bricks) #f (fourth bricks) (fifth bricks) (sixth bricks) (seventh bricks) (eighth bricks))]
    [(string=? collision-type "brick4") (list (first bricks) (second bricks) (third bricks) #f (fifth bricks) (sixth bricks) (seventh bricks) (eighth bricks))]
    [(string=? collision-type "brick5") (list (first bricks) (second bricks) (third bricks) (fourth bricks) #f (sixth bricks) (seventh bricks) (eighth bricks))]
    [(string=? collision-type "brick6") (list (first bricks) (second bricks) (third bricks) (fourth bricks) (fifth bricks) #f (seventh bricks) (eighth bricks))]
    [(string=? collision-type "brick7") (list (first bricks) (second bricks) (third bricks) (fourth bricks) (fifth bricks) (sixth bricks) #f (eighth bricks))]
    [(string=? collision-type "brick8") (list (first bricks) (second bricks) (third bricks) (fourth bricks) (fifth bricks) (sixth bricks) (seventh bricks) #f)]
    [else bricks]))

; Die Steuerung für die Bar
(define (keyHandler world key)
  (if (world-game-over? world)
      world
      (make-world (world-ball world)
                  (cond [(or (key=? key "a") (key=? key "left"))
                         (max (/ BAR-WIDTH 2) (- (world-bar-x world) 10))]
                        [(or (key=? key "d") (key=? key "right"))
                         (min (- WIDTH (/ BAR-WIDTH 2)) (+ (world-bar-x world) 10))]
                        [else (world-bar-x world)])
                  #f
                  #t
                  (world-bricks world))))

; überprüft, ob alle Ziegelsteine zerstört wurden
(define (all-bricks-destroyed? bricks)
  (all-false? bricks))

; überprüft, ob alle Elemente einer Liste falsch sind  
(define (all-false? lst)
  (cond
    [(empty? lst) #t]
    [(first lst) #f]
    [else (all-false? (rest lst))]))

; überprüft, ob das Spiel zu Ende ist
(define (end-of-the-world? world)
  (world-game-over? world))

; Initialer Zustand der Welt
(define INITIAL-WORLD
  (make-world (make-ball (make-posn (/ WIDTH 2) (- HEIGHT 250))
                         (make-vel 1 3))
              (/ WIDTH 2)
              #f
              #f
              (list #t #t #t #t #t #t #t #t)))

; Hauptprogramm
(define (main ws)
  (big-bang ws
            (on-tick tick 0.01)
            (to-draw render)
            (on-key keyHandler)
            (stop-when end-of-the-world? render)))

; Starten des Spiels
(main INITIAL-WORLD)
