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

(struct player (magni veloc locat) #:transparent)
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
(define ex-p ; a player moving at 0,1 velocity, 0° rotation, 100,100 units from origin
  (player 1 (pvec 0 1) (pvec 100 100)))

(define ex-p2
  (player 100 (pvec 0 1) (pvec 360 360)))

; -------------------------------------------------------------------------------------
#| PHYSICAL CONSTANTS |#
; -------------------------------------------------------------------------------------
(define WIDTH  720)
(define HEIGHT 720)
(define TURN-RATE (/ pi 6))
(define MAX-SPEED 10)

; -------------------------------------------------------------------------------------
#| VISUAL CONSTANTS |#
; -------------------------------------------------------------------------------------
(define BACKG (empty-scene WIDTH HEIGHT))
;(define SHIP (triangle 25 'solid 'thistle))
(define SHIP (star-polygon (/ 720 15) 5 1 'solid 'thistle))
(define TURRET (circle 5 'solid 'orange))

; -------------------------------------------------------------------------------------
#| INITIALIZATION |#
; ------------------------------------------------------------------------------------
; -> Player
; the game's state, for now, is just a player
(define (start)
  (big-bang (init-game)
            [on-key direct-ship]
            [on-tick fly-ship]
            [to-draw render-game]
            [state #t]))

; -> Game
(define (init-game)
  (game (init-player) (init-turret)))

; -> Player
(define (init-player)
  (player 0 (pvec 0 0) (pvec (random WIDTH) (random HEIGHT))))

; -> Turret
(define (init-turret)
  (pvec (random WIDTH) (random HEIGHT)))

; -------------------------------------------------------------------------------------
#| LOGIC |#
; -------------------------------------------------------------------------------------
(define (sin~ n)
  (round (sin n)))

(define (cos~ n)
  (round (cos n)))

; Pvec Pvec -> Pvec
; add 2 vectors together
(define (vec+ v1 v2)
  (pvec
   (+ (pvec-x v1) (pvec-x v2))
   (+ (pvec-y v1) (pvec-y v2))))

; Pvec Pvec -> Rational
; returns the heading of a line between two vector points in radians
(define (heading v1 v2)
    ; take the inverse tangent of the difference between two points
  (atan (/ (- (pvec-y v2) (pvec-y v1))
           (- (pvec-x v2) (pvec-x v1)))))

; Radians -> Degrees
(define (rad->deg r)
  (/ (* r 180) pi))

; Pvec Pvec -> Pvec
(define (vec-max v1 v2)
  (define origin (pvec 0 0))
  (if (>= (vec-mag v1 origin)
          (vec-mag v2 origin))
      v1
      v2))

; Pvec Pvec -> Pvec
(define (vec-min v1 v2)
  (define origin (pvec 0 0))
  (if (< (vec-mag v1 origin)
         (vec-mag v2 origin))
      v1
      v2))

; Pvec Pvec -> Pvec
(define (vec- v1 v2)
  (pvec
   (- (pvec-x v1) (pvec-x v2))
   (- (pvec-y v1) (pvec-y v2))))

; Pvec Pvec -> Number
; b/c we represent points as 2D vectors as well, our magnitude must take this into account
(define (vec-mag v1 v2)
  (sqrt (+ (sqr (- (pvec-x v1) (pvec-x v2)))
           (sqr (- (pvec-y v1) (pvec-y v2))))))

; later on, add some other vector functions that will be useful, like computing the heading as an angle, computing it's magnitude, etc.
; Game KeyEvent -> Game
(define (direct-ship g ke)
  (define p (game-p g))
  (define vel (player-veloc p))
  (define loc (player-locat p))
  (define s (player-magni p))
  (game
   (match ke ; you'll have to change the velocity
    ; the velocity could be: an integer, represnting speed, which is then applied
    ; to the vector representing the heading?
    ; or: a vector representing speed, add (cos TURN-RATE) (sin TURN-RATE) when you turn
    ["left" (move left p)]
    ["right" (move right p)]
    ["up"
     (player (intrvl add1 s)
             (vec+ vel (pvec 0 (sin~ (* TURN-RATE (add1 s)))))
             loc)]
    ["down"
     (player (intrvl sub1 s)
             (vec- vel (pvec 0 (sin~ (* TURN-RATE (sub1 s)))))
             loc)]
    [_ p])
   (game-t g)))

(define (intrvl proc n)
  (define new (proc n))
  (cond
    [(< new 0) 0]
    [(> new MAX-SPEED) MAX-SPEED]
    [else new]))

(define (move dir pl)
  (define s (player-magni pl))
  (define loc (player-locat pl))
  (player s
          (dir (player-veloc pl) s)
          loc))

(define (left vel s)
  ; this is broken!
  (vec- vel (pvec (* s (cos~ (* TURN-RATE s)))
                  (* s (sin~ (* TURN-RATE s))))))

(define (right vel s)
  (vec+ vel (pvec (* s (cos~ (* TURN-RATE s)))
                  (* s (sin~ (* TURN-RATE s))))))

; Pvec [Pvec -> Pvec] Pvec -> Pvec
(define (update-nloc orig p-or-m new-vec)
  (p-or-m orig new-vec))

; Game -> Game
(define (fly-ship g)
  (define p (game-p g))
  (define vel (player-veloc p))
  (define loc (player-locat p))
  ; - IN -
  (game
   (struct-copy player p [locat (vec+ loc vel)])
   (game-t g)))

; -------------------------------------------------------------------------------------
#| RENDERING |#
; -------------------------------------------------------------------------------------
; Player -> Image
(define (render-game g)
  (define p (game-p g))
  (place-image
   (rotate (rad->deg (heading (player-veloc p) (player-locat p))) SHIP)
   (pvec-x (player-locat p))
   (pvec-y (player-locat p))
   BACKG))
