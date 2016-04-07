#lang racket
(require 2htdp/image 2htdp/universe)
; -------------------------------------------------------------------------------------
#| DATA |#
; -------------------------------------------------------------------------------------
(struct player (accel rotat locat))
; player = (player Integer Integer Polar)
; in (player a r l)
; -- a is the rate at which the player is accelerating,
;    positive for faster, negative for slower
; -- r is the player's current rotation, used for determining direction and rendering
;   positive is counter-clockwise and negative is clockwise
; -- l is the player's location, expressed as an angle and magnitude from the upper left

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
; Player KeyEvent -> Player
(define (direct-ship p ke)
  p)

; Player -> Player
(define (fly-ship p)
  (make-magnitude
   ; what if we're flying straight up?
   ; changing this to a physical vector instead of a polar number might work out much better for the acceleration model of the game
   ; oh, boopo.
   ; boopo, boopo!
   (+ (magnitude p) (player-accel p))
   (angle p)))

; -------------------------------------------------------------------------------------
#| RENDERING |#
; -------------------------------------------------------------------------------------
; Player -> Image
; if the player's location is a polar coord, we have to deconstruct into cartesian points
(define (render-game p)
  (place-image
   (rotate (player-rotat p) SHIP)
   (polar->cartes (player-locat p))
   BACKG))

; Polar -> Number Number
(define (polar->cartes num)
  (values (round (* (magnitude num) (cos (angle num))))
          (round (* (magnitude num) (sin (angle num))))))
