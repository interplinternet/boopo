#lang racket
(require 2htdp/image 2htdp/universe "quadtree.rkt")

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
;(define BACKG (empty-scene WIDTH HEIGHT))
(define SPACE (bitmap/file "graphics/big_space.bmp"))
(define BACKG SPACE)
;(define SHIP (star-polygon (/ 720 15) 5 1 'solid 'thistle))
;(define SHIP (triangle/sss 50 50 20 'solid 'thistle))
(define SHIP (rotate 180 (bitmap/file "graphics/player-ship.png")))
(define TURRET (circle 15 'solid 'orange))
(define PWIDTH (image-width SHIP))
(define PHEIGHT (image-height SHIP))
(define BWIDTH (image-width BACKG))
(define BHEIGHT (image-height BACKG))
(define BXCENTER (/ BWIDTH 2))
(define BYCENTER (/ BHEIGHT 2))
(define INIT_X (/ BWIDTH 4)) ; the initial x and y coordinates on the background
(define INIT_Y (/ BHEIGHT 4))

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

;(struct player (magni veloc locat turns) #:transparent)
(struct player entity (magni veloc turns) #:transparent)
; player = (player Integer Integer Polar)
; in (player a v l)
; -- a is the magnitude of the player's velocity
; -- v is the player's velocity
; -- l is the player's location

(struct game (p t q) #:transparent)
; (game Player Entity Quadtree)
; contains the player, the turret, and a quadtree for collision detection.

; A turret could be:
; 1. a pvec, representing it's x- and y-coords.
; 2. a struct, containing a pvec and it's rotation and firing information

; -------------------------------------------------------------------------------------
#| DATA EXAMPLES |#
; -------------------------------------------------------------------------------------
#;(define ex-p
    (player (entity (posn 360 360) (image-width SHIP) (image-height SHIP))
            0 (pvec 0 10) 0))
(define ex-p
  (player (posn 360 360) PWIDTH PHEIGHT 1 (pvec 0 10) 0))

; -------------------------------------------------------------------------------------
#| INITIALIZATION |#
; ------------------------------------------------------------------------------------
; -> Game
(define (start)
  (define the-game (init-game))
  (define NODE (game-q the-game))

  (big-bang (init-game)
            [on-key direct-game]
            [on-tick update-game 1/28]
            [to-draw render-game]
            [state #t]))

; -> Game
(define (init-game)
  (define turret (init-turret))
  ; the game doesn't insert the player into the quadtree until after they move,
  ; the turret is inserted before play b/c it never moves and can act as a
  ; "base" node for re-insertion, so we don't have to constantly reinsert it.
  (game (init-player) turret (insert-node VROOT turret WIDTH)))

; -> Player
(define (init-player)
  (player
   (posn 360 360) PWIDTH PHEIGHT
   0 (pvec 1 0) INIT-TURN))
;(player 0 (pvec 0 0) (pvec (random WIDTH) (random HEIGHT))))

; -> Turret
(define (init-turret)
  (entity (posn (random WIDTH) (random HEIGHT))
          (image-width TURRET) (image-height TURRET)))

; Game KeyEvent -> Game
(define (direct-game g ke)
  (define p (game-p g))
  (define current-quad (game-q g))
  (define new-ship (direct-ship p ke))
  (define new-turret (direct-turret (game-t g)))

  (game new-ship new-turret
        (cond
          [(exceeds-quad? new-ship current-quad)
           (insert-node VROOT new-ship WIDTH)]
          [else current-quad])))

; Game -> Game
(define (update-game g)
  (collect-garbage 'incremental)
  (define new-ship (fly-ship (game-p g)))
  (game new-ship
        (rotate-turret (game-t g))
        (if (zero? (player-magni new-ship))
            (game-q g)
            (insert-node VROOT new-ship WIDTH))))
; -------------------------------------------------------------------------------------
#| LOGIC |#
; -------------------------------------------------------------------------------------
(define (sin~ n)
  (rationalize (sin n) .1))

(define (cos~ n)
  (rationalize (cos n) .1))

; Posn Vector -> Posn
(define (loc+ p v)
  (posn
   (rationalize (+ (posn-x p) (pvec-x v)) .1)
   (rationalize (+ (posn-y p) (pvec-y v)) .1)))

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

; Player Node -> Boolean
; consumes a player and a quadtree, determines if the player has moved outside
; of their current quadrant.
; Isn't this pretty inefficient? You'd have to walk the quad, pull the
; appropriate node, then compare the ship's coordinates (after adding the ship's
; width and height) to the bounds of that quad
(define (exceeds-quad? ship quad)
  #t)

; Player -> Player
(define (direct-ship pl ke)
  (define s (player-magni pl))
  (define t (player-turns pl))
  (match ke
    ["left"  (turn pl (modulo (add1 t) MAX-TURNS))]
    ["right" (turn pl (modulo (sub1 t) MAX-TURNS))]
    ["up"    (struct-copy player pl [magni (intrvl add1 s)])]
    ["down"  (struct-copy player pl [magni (intrvl sub1 s)])]
    ["r"     (player (posn 360 360) PWIDTH PHEIGHT 0 (pvec 1 0) INIT-TURN)]
    [_ pl]))

; Turret -> Turret
; for now, there is nothing you can do with the turret. If it turns out that
; there's never anything to do with it, I'll just remove this.
(define (direct-turret tr)
  tr)

; [Number -> Number] Number -> Number
(define (intrvl proc n)
  (define new (proc n))
  (cond
    [(< new 0) 0]
    [(> new MAX-SPEED) MAX-SPEED]
    [else new]))

; Player Number -> Player
(define (turn pl turn#)
  (struct-copy player pl
               [veloc (pvec (cos~ (* TURN-RATE turn#))
                            (sin~ (* TURN-RATE turn#)))]
               [turns turn#]))



; Player -> Player
(define (fly-ship pl)
  (define vel     (player-veloc pl))
  (define loc     (entity-coord pl))
  (define s       (player-magni pl))
  (define new-loc (loc+ loc (rotate-quad (vec-scale vel s))))
  ; - IN -
  (if (and (<= 0 (posn-x new-loc) WIDTH)
           (<= 0 (posn-y new-loc) HEIGHT))
      ;(struct-copy player pl [coord new-loc])
      #;(player (struct-copy entity pl [coord new-loc])
                (player-magni pl) (player-veloc pl) (player-turns pl))
      (player new-loc PWIDTH PHEIGHT
              (player-magni pl) (player-veloc pl) (player-turns pl))
      pl))

; Turret -> Turret
(define (rotate-turret tr)
  tr)

; Pvec -> Pvec
; rotates the quadrant in a cartesian plane a vector is in
; in accordance with how racket interprets negative and positive movement.
(define (rotate-quad vec)
  (match vec
    [(pvec x y) (pvec x (- y))]))

; -------------------------------------------------------------------------------------
#| RENDERING |#
; -------------------------------------------------------------------------------------
; Game -> Image
(define (render-game g)
  (render-ship (game-p g)
               (render-turret (game-t g)
                              (render-bg (game-p g) BACKG))))

; Player Image -> Image
(define (render-ui pl im)
  (overlay/align 'center 'bottom
                 (text (number->string (player-magni pl)) 20 'black)
                 im))

; Player Image -> Image
(define (render-ship pl im)
  (place-image
   (rotate (rationalize
            (+ 90 (* (radians->degrees TURN-RATE) (player-turns pl))) .5) SHIP)
   (posn-x (entity-coord pl))
   (posn-y (entity-coord pl))
   im))

; Turret Image -> Image
(define (render-turret tr im)
  (define crds (entity-coord tr))
  (place-image TURRET (posn-x crds) (posn-y crds) im))

; Player Image -> Image
(define (render-bg pl im)
  (define coords (entity-coord pl))
  (scroll im
          (posn-x coords)
          (posn-y coords)
          (/ (difference (posn-x coords) 360)
             360)
          (/ (difference (posn-y coords) 360)
             360)))

; Number Number -> Number
; consumes two numbers and produces the difference between them
(define (difference n m)
  (abs (- n m)))

; Posn Posn -> Number
; determines the distance between two cartesian points. Using this instead
; of taking the "straight" difference between x/y and 360 leads to some odd
; results corresponding to sine/cosine motions around the circle.
(define (distance point origin)
  (sqrt (+ (sqr (- (posn-x point) (posn-x origin)))
           (sqr (- (posn-x point) (posn-y origin))))))

; Image Number-> Image
; scrolls a background left by x and up by y
; given a background image, a current upper-left corner,
; and an amount to scroll by:
; return a new image resulting from translating the given corner by x and y
(define (scroll bg x-coord y-coord x-rate y-rate)
  (define new-x (+ INIT_X (* (- x-coord 360) x-rate)))
  (define new-y (+ INIT_Y (* (- y-coord 360) y-rate)))
  ; - IN -
  (crop new-x new-y WIDTH HEIGHT bg))

; [Listof Number] -> Image
; takes in a list of information, transforms it into strings, then appends
; labels to it, then flattens it into a single string and renders it.
(define (render-info info*)
  (define lo-str (map number->string info*))
  (define label+info (map (λ (label info) (string-append label " " info " "))
                          (list "cur-x:" "cur-y:")
                          lo-str))
  (text (apply string-append label+info) 20 'black))
#| (text
(for/fold ([str_acc ""])
          ([str-info (in-list info*)]
           [str-label (in-list '("cur-x:" "cur-y:"))])
  (string-append str-label " " (number->string str-info) " " str_acc)) 20 'black) |#

; Number X [Listof [X -> Y]] -> [Listof Benchmarks]
(define (benchmark-funcs tm input . funcs)
  (for ([func funcs])
    (displayln func)
    (time (for ([i (in-range tm)])
            (func input)))))
#;(define start-game
    (thread start))
