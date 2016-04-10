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
  (player 0 (pvec 0 0) (pvec 360 360) 0))
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

; Pvec Pvec -> Pvec
; add 2 vectors together
(define (vec+ v1 v2)
  (pvec
   (rationalize (+ (pvec-x v1) (pvec-x v2)) .1)
   (rationalize (+ (pvec-y v1) (pvec-y v2)) .1)))

; Pvec Number -> Pvec
; scales a vector by a scalar
(define (vec-scale v s)
  (pvec (rationalize (* (pvec-x v) s) .1)
        (rationalize (* (pvec-y v) s) .1)))

; Pvec Pvec -> Rational
; returns the heading of a line between two vector points in radians
(define (heading v1 v2)
  ; take the inverse tangent of the difference between two points
  (define dy (- (pvec-y v2) (pvec-y v1)))
  (define dx (- (pvec-x v2) (pvec-x v1)))
  (if (and (zero? dy) (zero? dx))
      0
      (rationalize (atan (- (pvec-y v2) (pvec-y v1))
                         (- (pvec-x v2) (pvec-x v1))) .1)))
  #| (atan (/ (- (pvec-y v2) (pvec-y v1))
           (- (pvec-x v2) (pvec-x v1))))) |#

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
   (rationalize (- (pvec-x v1) (pvec-x v2)) .1)
   (rationalize (- (pvec-y v1) (pvec-y v2)) .1)))

; Pvec Pvec -> Number
; b/c we represent points as 2D vectors as well,
; our magnitude must take this into account
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
  (define t (player-turns p))
  (game
   (match ke ; you'll have to change the velocity
    ; the velocity could be: an integer, represnting speed, which is then applied
    ; to the vector representing the heading?
    ; or: a vector representing speed, add (cos TURN-RATE) (sin TURN-RATE) when you turn
    ["left" (move left (struct-copy player p [turns (modulo (add1 t) 12)]))]
    ["right" (move right (struct-copy player p [turns (modulo (sub1 t) 12)]))]
    ["up" ;(struct-copy player p [veloc (vec-scale vel (add1 s))] [magni (intrvl add1 s)])]
     (struct-copy player p
                  [veloc (vec+ vel (pvec 0 1))]
                  [magni (intrvl add1 s)])]
    #| (player (intrvl add1 s)
             (vec+ vel (pvec 0 (sin~ (* TURN-RATE (add1 s)))))
             loc)] |#
    ["down"
     (struct-copy player p
                  [veloc (vec-scale vel (intrvl sub1 s))]
                  [magni (intrvl sub1 s)])]
    #| (player (intrvl sub1 s)
             (vec- vel (pvec 0 (sin~ (* TURN-RATE (sub1 s)))))
    loc)] |#
    ["r" (player 0 (pvec 0 0) (pvec 360 360) 0)]
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
  (define t (player-turns pl))
 #| (player s
          (dir (player-veloc pl) t s)
          loc
  t)) |#
  (struct-copy player pl [veloc (pvec (cos~ (* TURN-RATE t)) (sin~ (* TURN-RATE t)))]))

(define (left vel turns s)
  ; this is broken!
  ;(vec- vel (vec-scale (pvec (cos~ TURN-RATE) (sin~ TURN-RATE)) s)))
  (vec-scale (pvec (cos~ (* TURN-RATE turns))
                   (sin~ (* TURN-RATE turns))) s))

(define (right vel turns s)
  ;(vec+ vel (vec-scale (pvec (cos~ TURN-RATE) (sin~ TURN-RATE)) s)))
  (vec-scale (pvec (cos~ (* TURN-RATE turns))
                   (sin~ (* TURN-RATE turns))) s))

; Game -> Game
(define (fly-ship g)
  (define p (game-p g))
  (define vel (player-veloc p))
  (define loc (player-locat p))
  (define s (player-magni p))
  ; - IN -
  (game
   (if (and (<= 0 (pvec-x loc) WIDTH) (<= 0 (pvec-y loc) HEIGHT))
       (struct-copy player p [locat (vec- loc vel)])
       p)
   (game-t g)))

; -------------------------------------------------------------------------------------
#| RENDERING |#
; -------------------------------------------------------------------------------------
; Player -> Image
(define (render-game g)
  (define p (game-p g))
  (place-image
   #| (rotate (radians->degrees
            (rationalize (heading (vec+ (player-veloc p) (player-locat p))
                                  (player-locat p))
   .1)) SHIP) |#
   (rotate (rationalize (radians->degrees (* TURN-RATE (player-turns p))) .5) SHIP)
   (pvec-x (player-locat p))
   (pvec-y (player-locat p))
   BACKG))

(define start-game
  (thread start))
