#lang racket
(require 2htdp/image 2htdp/universe)
; -------------------------------------------------------------------------------------
#| DATA |#
; -------------------------------------------------------------------------------------
(struct pvec (x y) #:transparent)
; pvec = (pvec Rational Rational)
; in (pvec x y v)
; -- x is the x-val
; -- y is the y-val
; New location = velocity applied to current location. Velocity is also a vectory
; A location can be either a singular point
; or the vector representing the difference between location and origin

(struct player (magni veloc locat turns) #:transparent)
; player = (player Integer Integer Polar)
; in (player a v l)
; -- a is the magnitude of the player's velocity
; -- v is the player's velocity
; -- l is the player's location

(struct game (p t) #:transparent) ; contains a player and the turret

; A turret could be:
; 1. a pvec, representing it's x- and y-coords.
; 2. a struct, containing a pvec and it's rotation and firing information

; -------------------------------------------------------------------------------------
#| DATA EXAMPLES |#
; -------------------------------------------------------------------------------------
(define ex-p
  (player 10 (pvec 0 10) (pvec 360 360) 0))

; -------------------------------------------------------------------------------------
#| PHYSICAL CONSTANTS |#
; -------------------------------------------------------------------------------------
(define WIDTH  720)
(define HEIGHT 720)
(define TURN-RATE (/ pi 4))
(define MAX-TURNS (/ (* 2 pi) TURN-RATE))
(define MAX-SPEED 10)
(define INIT-TURN 0)

; -------------------------------------------------------------------------------------
#| VISUAL CONSTANTS |#
; -------------------------------------------------------------------------------------
(define BACKG (empty-scene WIDTH HEIGHT))
;(define SHIP (star-polygon (/ 720 15) 5 1 'solid 'thistle))
(define SHIP (triangle/sss 50 50 20 'solid 'thistle))
(define TURRET (circle 15 'solid 'orange))

; -------------------------------------------------------------------------------------
#| INITIALIZATION |#
; ------------------------------------------------------------------------------------
; -> Player
; the game's state, for now, is just a player
(define (start)
  (big-bang (init-game)
            [on-key direct-game]
            [on-tick update-game]
            [to-draw render-game]
            [state #t]))

; -> Game
(define (init-game)
  (game (init-player) (init-turret)))

; -> Player
(define (init-player)
  (player 0 (pvec 1 0) (pvec 360 360) INIT-TURN))
  ;(player 0 (pvec 0 0) (pvec (random WIDTH) (random HEIGHT))))

; -> Turret
(define (init-turret)
  (pvec (random WIDTH) (random HEIGHT)))

; -------------------------------------------------------------------------------------
#| LOGIC |#
; -------------------------------------------------------------------------------------
(define (sin~ n)
  (rationalize (sin n) .1))

(define (cos~ n)
  (rationalize (cos n) .1))

;;----------VECTORS----------;;
; Pvec Pvec -> Pvec
; add 2 vectors together
(define (vec+ v1 v2)
  (pvec
   (rationalize (+ (pvec-x v1) (pvec-x v2)) .1)
   (rationalize (+ (pvec-y v1) (pvec-y v2)) .1)))

; Pvec Pvec -> Pvec
(define (vec- v1 v2)
  (pvec
   (rationalize (- (pvec-x v1) (pvec-x v2)) .1)
   (rationalize (- (pvec-y v1) (pvec-y v2)) .1)))

; Pvec Number -> Pvec
; scales a vector by a scalar
(define (vec-scale v s)
  (pvec (rationalize (* (pvec-x v) s) .1)
        (rationalize (* (pvec-y v) s) .1)))

; Pvec Pvec -> Rational
; returns the heading of a line between two vector points in radians
; by taking the inverse tangent of the difference between two points
(define (heading v1 v2)
  (define dy (- (pvec-y v2) (pvec-y v1)))
  (define dx (- (pvec-x v2) (pvec-x v1)))
  (if (and (zero? dy) (zero? dx))
      0
      (rationalize (atan (- (pvec-y v2) (pvec-y v1))
                         (- (pvec-x v2) (pvec-x v1))) .1)))

; Pvec [Pvec 0 0] -> Number
; b/c we represent points as 2D vectors as well,
; our magnitude must take this into account
(define (vec-mag v1 [base (pvec 0 0)])
  (sqrt (+ (sqr (- (pvec-x v1) (pvec-x base)))
           (sqr (- (pvec-y v1) (pvec-y base))))))

; Pvec Pvec -> Pvec
; returns the largest of the two vectors
(define (vec-max v1 v2)
  (if (>= (vec-mag v1)
          (vec-mag v2))
      v1
      v2))

; Pvec Pvec -> Pvec
; returns the smallest of two vectors
(define (vec-min v1 v2)
  (if (< (vec-mag v1)
         (vec-mag v2))
      v1
      v2))
;;----------GAME----------;;
; Game KeyEvent -> Game
(define (direct-game g ke)
  (define p (game-p g))
  (game
   (direct-ship p ke)
   (direct-turret (game-t g))))

; Player -> Player
(define (direct-ship pl ke)
  (define s (player-magni pl))
  (define t (player-turns pl))
  (match ke
    ["left"  (move pl (modulo (add1 t) MAX-TURNS))]
    ["right" (move pl (modulo (sub1 t) MAX-TURNS))]
    ["up"    (struct-copy player pl [magni (intrvl add1 s)])]
    ["down"  (struct-copy player pl [magni (intrvl sub1 s)])]
    ["r"     (player 0 (pvec 1 0) (pvec 360 360) INIT-TURN)]
    [_ pl]))

; Turret -> Turret
; for now, there is nothing you can do with the turret. If it turns out that there's never
; anything to do with it, I'll just remove this.
(define (direct-turret tr)
  tr)

(define (intrvl proc n)
  (define new (proc n))
  (cond
    [(< new 0) 0]
    [(> new MAX-SPEED) MAX-SPEED]
    [else new]))

; Player Number -> Player
(define (move pl turn#)
  (struct-copy player pl
               [veloc (pvec (cos~ (* TURN-RATE turn#))
                            (sin~ (* TURN-RATE turn#)))]
               [turns turn#]))

; Game -> Game
(define (update-game g)
  (game (fly-ship (game-p g))
        (rotate-turret (game-t g))))

; Player -> Player
(define (fly-ship pl)
  (define vel     (player-veloc pl))
  (define loc     (player-locat pl))
  (define s       (player-magni pl))
  (define new-loc (vec+ loc (rotate-quad (vec-scale vel s))))
  ; - IN -
  (if (and (<= 0 (pvec-x new-loc) WIDTH) (<= 0 (pvec-y new-loc) HEIGHT))
      (struct-copy player pl [locat new-loc])
      pl))

; Turret -> Turret
(define (rotate-turret tr)
  tr)

; Pvec -> Pvec
; rotates the quadrant a vector is in in accordance with how racket interprets
; negative and positive movement.
(define (rotate-quad vec)
  (match vec
    [(pvec x y) (pvec x (- y))]))

; -------------------------------------------------------------------------------------
#| RENDERING |#
; -------------------------------------------------------------------------------------
; Player -> Image
(define (render-game g)
  (render-ship (game-p g)
               (render-turret (game-t g) BACKG)))

; Player Image -> Image
(define (render-ship pl im)
  (place-image
   (rotate (rationalize (+ 90 (* (radians->degrees TURN-RATE) (player-turns pl))) .5) SHIP)
   (pvec-x (player-locat pl))
   (pvec-y (player-locat pl))
   im))

; Turret Image -> Image
(define (render-turret tr im)
  (place-image TURRET (pvec-x tr) (pvec-y tr) im))

(define start-game
  (thread start))
