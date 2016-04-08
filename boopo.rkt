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

(struct player (veloc rotat locat) #:transparent)
; player = (player Integer Integer Polar)
; in (player a r l)
; -- a is the player's velocity
; -- r is the player's current rotation, used for determining direction and rendering
;   positive is counter-clockwise and negative is clockwise
;   is this really necessary? Since velocity is directed, isn't it redundant to
;   mention direction twice?
; -- l is the player's location, expressed as an angle and magnitude from the upper left
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

; -------------------------------------------------------------------------------------
#| VISUAL CONSTANTS |#
; -------------------------------------------------------------------------------------
(define BACKG (empty-scene WIDTH HEIGHT))
(define SHIP (triangle 25 'solid 'thistle))
(define TURRET (circle 5 'solid 'orange))

; -------------------------------------------------------------------------------------
#| INITIALIZATION |#
; -------------------------------------------------------------------------------------
; -> Player
; the game's state, for now, is just a player
(define (start)
  (big-bang (player 0 0 (random WIDTH))
            [on-key direct-ship]
            [on-tick fly-ship]
            [to-draw render-game]))

; -------------------------------------------------------------------------------------
#| LOGIC |#
; -------------------------------------------------------------------------------------
; Pvec Pvec -> Pvec
; add 2 vectors together
(define (vec+ v1 v2)
  (pvec
   (+ (pvec-x v1) (pvec-x v2))
   (+ (pvec-y v1) (pvec-y v2))))

; later on, add some other vector functions that will be useful, like computing the heading as an angle, computing it's magnitude, etc.
; Player KeyEvent -> Player
(define (direct-ship p ke)
  (define vel (player-veloc p))
  (define loc (player-locat p))
  (match ke
    ["left" (vec+ vel (pvec -1 0))]
    ["right" (vec+ vel (pvec 1 0))]
    ["up" (vec+ vel (pvec 0 1))]
    ["down" (vec+ vel (pvec 0 -1))]
    [_ p]))

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
   (rotate (player-rotat p) SHIP)
   (pvec-x (player-locat p))
   (pvec-y (player-locat p))
   BACKG))
