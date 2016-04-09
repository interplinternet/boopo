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

(struct player (veloc head locat) #:transparent)
; player = (player Integer Integer Polar)
; in (player a h l)
; -- a is the player's velocity
; -- h is the player's current heading, used for determining direction and rendering
;    This is an accumulated value calculated from the player's location and velocity.
; -- l is the player's location, expressed as an angle and magnitude from the upper left

(struct game (p t) #:transparent) ; contains a player and the turret

; A turret could be:
; 1. a pvec, representing it's x- and y-coords.
; 2. a struct, containing a pvec and it's rotation and firing information
; -------------------------------------------------------------------------------------
#| DATA EXAMPLES |#
; -------------------------------------------------------------------------------------
(define ex-p ; a player moving at 1,1 velocity, 0° rotation, 100,100 units from origin
  (player (pvec 1 1) 0 (pvec 100 100)))

; -------------------------------------------------------------------------------------
#| PHYSICAL CONSTANTS |#
; -------------------------------------------------------------------------------------
(define WIDTH  720)
(define HEIGHT 720)
(define TURN-RATE (/ pi 6))
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
            [to-draw render-game]))

; -> Game
(define (init-game)
  (game (init-player) (init-turret)))

; -> Player
(define (init-player)
  (player (pvec 0 0) 0 (pvec (random WIDTH) (random HEIGHT))))

; -> Turret
(define (init-turret)
  (pvec (random WIDTH) (random HEIGHT)))

; -------------------------------------------------------------------------------------
#| LOGIC |#
; -------------------------------------------------------------------------------------
; Pvec Pvec -> Pvec
; add 2 vectors together
(define (vec+ v1 v2)
  (pvec
   (+ (pvec-x v1) (pvec-x v2))
   (+ (pvec-y v1) (pvec-y v2))))

; Pvec Pvec -> Rational
; returns the heading of a line between two vector points in radians
(define (heading v1 v2)
  ; - IN -
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
; Player KeyEvent -> Player
(define (direct-ship p ke)
  (define vel (player-veloc p))
  (define oloc (player-locat p))
  (define nloc (player-head p))
  (match ke ; you'll have to change the velocity
    ; the velocity could be: an integer, represnting speed, which is then applied
    ; to the vector representing the heading?
    ; or: a vector representing speed, add (cos TURN-RATE) (sin TURN-RATE) when you turn
    ["left"
     (player (vec+ vel (pvec -1 0))
             (update-nloc nloc vec+ (pvec (cos TURN-RATE) (sin TURN-RATE))) oloc)]
    ["right"
     (player (vec+ vel (pvec 1 0))
             (update-nloc nloc vec- (pvec (cos TURN-RATE) (sin TURN-RATE))) oloc)]
    ["up"
     (player (vec+ vel (pvec 0 -1)) nloc oloc)]
    ["down"
     (player (vec+ vel (pvec 0 1)) nloc oloc)]
    [_ p]))

; Pvec [Pvec -> Pvec] Pvec -> Pvec
(define (update-nloc orig p-or-m new-vec)
  (p-or-m orig new-vec))

; Player -> Player
(define (fly-ship p)
  (define vel (player-veloc p))
  (define loc (player-locat p))
  ; - IN -
  (struct-copy player p [locat (vec+ loc vel)]))

; -------------------------------------------------------------------------------------
#| RENDERING |#
; -------------------------------------------------------------------------------------
; Player -> Image
(define (render-game p)
  (place-image
   (rotate (player-head p) SHIP)
   (pvec-x (player-locat p))
   (pvec-y (player-locat p))
   BACKG))
