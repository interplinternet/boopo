#lang racket
(require 2htdp/image 2htdp/universe "quadtree.rkt" racket/trace racket/vector)

; -------------------------------------------------------------------------------------
#| PHYSICAL CONSTANTS |#
; -------------------------------------------------------------------------------------
(define WIDTH  720)
(define HEIGHT 720)
(define TURN-RATE (/ pi 4))
(define MAX-TURNS (/ (* 2 pi) TURN-RATE))
(define MAX-SPEED 10)
(define INIT-TURN 0)
(define MAX-OBST 12)
(define MIN-OBST 6)
(define VROOT (node (posn 0 0) '() #()))
(bounds-set! WIDTH)
(define TICKRATE 1/28)
(define FIRERATE 3)
(define TIME-TO-FIRE (* TICKRATE FIRERATE))
(define MSPEED 10)
; -------------------------------------------------------------------------------------
#| VISUAL CONSTANTS |#
; -------------------------------------------------------------------------------------
(define SPACE    (bitmap/file "graphics/big_space.bmp"))
(define BACKG    (crop 0 0 720 720 SPACE))
(define SHIP     (rotate -90 (bitmap/file "graphics/player-ship.png")))
(define TURRET   (rotate 270 (overlay/align 'right 'center
                                            (circle 15 'solid 'orange)
                                            (rectangle 60 15 'solid 'orange))))
(define MRADIUS 5)
(define MISSILE (circle MRADIUS 'solid 'yellow))
(define PWIDTH   (image-width SHIP))
(define PHEIGHT  (image-height SHIP))
(define BWIDTH   (image-width BACKG))
(define BHEIGHT  (image-height BACKG))
(define BXCENTER (/ BWIDTH 2))
(define BYCENTER (/ BHEIGHT 2))
(define INIT_X   (/ BWIDTH 4)) ; the initial x and y coordinates on the background
(define INIT_Y   (/ BHEIGHT 4))

; -------------------------------------------------------------------------------------
#| DATA |#
; -------------------------------------------------------------------------------------
(struct pvec (x y) #:transparent)
; pvec = (pvec Rational Rational)
; in (pvec x y v)
; -- x is the x-val
; -- y is the y-val

;(struct player (magni veloc locat turns) #:transparent)
(struct player entity (magni veloc turns) #:transparent)
; player = (player Integer Integer Polar)
; in (player a v l)
; -- a is the magnitude of the player's velocity
; -- v is the player's velocity
; -- l is the player's location

(struct game (p t o q) #:transparent)
; (game Player Entity [Listof Entity] Quadtree)
; contains the player, the turret, the list of obstacles,
; and a quadtree for collision detection.

(struct turret entity (rotat fire))
; turret = (player (posn Rational Rational) Integer Integer Integer Rational Integer
; in (turret posn r f)
; -- Posn is the turret's location.
; -- R is the turret's current rotation, in radians.
; -- F is either a number between 84 and 0, signifying the countdown till firing,
;    or a list of fired projectiles.

(struct projectile entity (magni veloc))
; A projectile is an entity which is directed in some direction at some speed.
; Projectile = (projectile Posn Number Number Integer Rational)
; in (projectile p width height m v)
; -- m is the magnitude of the projectile, i.e., its speed
; -- v is its velocity, represented as a pair of rational numbers signifying a
; position along the unit circle, which is then scaled by its speed every turn.

; -------------------------------------------------------------------------------------
#| DATA EXAMPLES |#
; -------------------------------------------------------------------------------------
(define ex-p
  (player (posn 360 360) PWIDTH PHEIGHT
          1 (pvec 0 1) 0))
(define ex-node
  (node
   (posn 0 0)
   (list (entity (posn 345 318) 26 32) (turret (posn 351 508) 30 60 0 0))
   (vector
    (node (posn 0 0) '() '#())
    (node
     (posn 360 0)
     (list
      (entity (posn 483 231) 10 23)
      (entity (posn 508 108) 13 18)
      (entity (posn 500 287) 16 26))
     (vector
      (node (posn 360 0) '() '#())
      (node (posn 540 0) (list (entity (posn 696 56) 16 15)) '#())
      (node
       (posn 360 180)
       (list
        (player
         (posn 465.0 316.0)
         69
         43
         2
         (pvec 0.6666666666666666 0.6666666666666666)
         1.0))
       '#())
      (node (posn 540 180) '() '#())))
    (node (posn 0 360) (list (entity (posn 248 625) 13 20)) '#())
    (node
     (posn 360 360)
     (list (entity (posn 540 363) 21 20) (entity (posn 399 463) 32 15))
     '#()))))
; -------------------------------------------------------------------------------------
#| INITIALIZATION |#
; ------------------------------------------------------------------------------------
; -> Game
(define (start)
  (define the-game (init-game))
  (define NODE     (game-q the-game))
  (define obst-img (render-obst (game-o the-game) (empty-scene WIDTH HEIGHT 'transparent)))

  ; wrap the handlers with another function so they can use the base NODE with
  ; the turret to insert into.
  (define (update-with-base game)
    (update-game game NODE))
  (define (direct-with-base game key)
    (direct-game game key NODE))

  ; wraps the renderer with another function, so it doesn't have to re-render
  ; the static obstacles every-time
  (define (render-with-base game)
    (render-g game obst-img))

  (big-bang the-game
            [on-key    direct-with-base]
            [on-tick   update-with-base TICKRATE]
            [to-draw   render-with-base]
            [stop-when game-over?]
            [state     #f]))

; -> Game
(define (init-game)
  (define turret    (init-turret))
  (define obstacles (init-obst))
  ; The posn of every obstacle is "absolute", whereas the quadtree is relative.
  ; That means obstacles are never inserted into the appropriate node! They're often
  ; beyond the bounds of the the quad entirely, which is the WIDTH of the screen, not the
  ; entire background which scrolls and changes. The other problem is in scrolling. If we
  ; fold all obstacles into the quadtree at once, then they never change as they form the
  ; new root (this problem also exists with the turret). However, as the background
  ; scrolls, the obstacles and turrets are no longer in the same quads as they were in the
  ; beginning! Scrolling backgrounds, while pretty, create a disparity between the
  ; absolute of the background image and the relative coordinates of the quad.
  ; Solutions? Reinsert everything every tick. Or create a single "absolute" quadtree
  ; which is done over the entire background image, of which the player's screen is only a
  ; small part.
  (define base      (insert-node VROOT turret WIDTH))
  (define new-root  (foldl (λ (an-entity tree) (insert-node tree an-entity WIDTH))
                           base
                           obstacles))
  ; the game doesn't insert the player into the quadtree until after they move,
  ; the turret is inserted before play b/c it never moves and can act as a
  ; "base" node for re-insertion, so we don't have to constantly reinsert it.
  (game (init-player) turret obstacles new-root))

; -> Player
(define (init-player)
  (player
   (posn 360 360) PWIDTH PHEIGHT
   0 (pvec 1 0) INIT-TURN))

; -> Turret
(define (init-turret)
  (turret
   (posn (random WIDTH) (random HEIGHT))
   (image-width TURRET) (image-height TURRET)
   0
   TIME-TO-FIRE))

; -> [Listof Entity]
; returns a list containing random number of entity between n and m
(define (init-obst)
  (define num-of-obst (random MIN-OBST MAX-OBST))
  (gen-obst num-of-obst))

; Number -> [Listof Entity]
; Generate num amount of obstacles, but none may collide with one another.
; 1. Create an obstacle at a random position. Does it collide with any other obstacle so far?
; 2. If so, recreate it somewhere else. If not, cons it onto the list of obstacles so far.
(define (gen-obst n0)
  ; Number [Listof Entity] -> [Listof Entity]
  ; The accumulator represents all the obstacles generated so far.
  (define (gen-list num obstacles)
    (cond
      [(zero? num) obstacles]
      [else
       (define candidate (gen-random-obst))
       (if (ormap (λ (obst) (collides? candidate obst)) obstacles)
           (gen-list num obstacles)
           (gen-list (sub1 num) (cons candidate obstacles)))]))
  ; - IN -
  (gen-list n0 '()))

; Entity Entity -> Boolean
; Maybe I can use the quadtree for this. If two generated obstacles
; aren't in the same node, they can't collide.
(define (collides? candidate obst)
  (or (overlaps? candidate obst)
      (overlaps? obst candidate)))

; Entity Entity -> Boolean
; check if the second object overlaps the first
(define (overlaps? e1 e2)
  (match-define (entity (posn x1 y1) width1 height1) e1)
  (match-define (entity (posn x2 y2) width2 height2) e2)

  ; Number Number -> Number Number
  (define (+-half center width)
    (values (- center (/ width 2))
            (+ center (/ width 2))))
  (define-values (left1 right1) (+-half x1 width1))
  (define-values (top1 bot1)    (+-half y1 height1))
  (define-values (left2 right2) (+-half x2 width2))
  (define-values (top2 bot2)    (+-half y2 height2))
  ; - IN -
  (and (or (<= left1 left2 right1) (<= left1 right2 right1))
       (or (<= top1 top2 bot1)     (<= top1 bot2 bot1))))

; -> Entity
(define (gen-random-obst)
  ; a rectangle has an x- & y-coord, and a width and height.
  (define width (random 6 100))
  (define height (random 6 100))
  (entity (posn (random WIDTH)
                (random HEIGHT))
          width
          height))
; -------------------------------------------------------------------------------------
#| LOGIC |#
; -------------------------------------------------------------------------------------
(define (sin~ n)
  (rationalize (sin n) .1))

(define (cos~ n)
  (rationalize (cos n) .1))

;;----------VECTORS----------;;
; Pvec Pvec [Pvec -> Pvec] -> Pvec
(define/match (vec-oper v1 v2 func)
  [((pvec x1 y1) (pvec x2 y2) func)
   (pvec (rationalize (func x1 x2) .1)
         (rationalize (func y1 y2) .1))])

; Pvec Pvec -> Pvec
; add 2 vectors together
(define (vec+ v1 v2)
  (vec-oper v1 v2 +))

; Pvec Pvec -> Pvec
(define (vec- v1 v2)
  (vec-oper v1 v2 -))

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

; Posn Vector -> Posn
(define (loc+ p v)
  (posn
   (rationalize (+ (posn-x p) (pvec-x v)) .1)
   (rationalize (+ (posn-y p) (pvec-y v)) .1)))
;;----------GAME----------;;
; Game KeyEvent Node-> Game
(define (direct-game g ke root)
  (define p (game-p g))
  (define current-quad (game-q g))
  (define new-ship (direct-ship p ke))

  (struct-copy game g
               [p new-ship]
               [q (insert-node root new-ship WIDTH)]))

; Game -> Game
(define (update-game g root)
  (collect-garbage 'incremental)
  (match-define
    (game pl tr ob qd) g)
  (define new-ship (fly-ship pl qd))
  (define ship-in-quad (insert-node root new-ship WIDTH))
  (define projectile-or-num (turret-fire tr))
  (game new-ship
        (update-turret tr pl)
        ob
        (if (projectile? projectile-or-num)
            (insert-node ship-in-quad projectile-or-num WIDTH)
            ship-in-quad)))

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
    [_       pl]))

; [Number -> Number] Number -> Number
(define (intrvl proc n)
  (define new (proc n))
  (cond
    [(< new 0)         0]
    [(> new MAX-SPEED) MAX-SPEED]
    [else              new]))

; Player Number -> Player
(define (turn pl turn#)
  (struct-copy player pl
               [veloc (pvec (cos~ (* TURN-RATE turn#))
                            (sin~ (* TURN-RATE turn#)))]
               [turns turn#]))

; Node Player -> [Maybe Node]
; walks the tree and returns the node which contains the player
(define (players-node pl qd)
  (define (search-child* child*)
    (for/or ([a-child (in-vector child*)])
      (define candidate (players-node pl a-child))
      (if (node? candidate) candidate #f)))
  ; - IN -
  (cond
    [(member pl (node-content qd)) qd]
    [else
     (define candidate (search-child* (node-children qd)))
     (if (node? candidate) candidate #f)]))

; Player Node -> Player
(define (fly-ship pl quad)
  (match pl
    [(player pos width height speed veloc turns)
     (=> out-of-bounds)
     (define new-loc (loc+ pos (rotate-quad (vec-scale veloc speed))))
     (define new-pl (struct-copy player pl [coord #:parent entity new-loc]))
     (if (and (<= 0 (posn-x new-loc) WIDTH) (<= 0 (posn-y new-loc) HEIGHT)
              (no-collisions? new-pl (players-node new-pl quad)))
         new-pl
         (out-of-bounds))]
    [_ pl]))

; Player Node -> Boolean better collision detection might combine this with players-node.
; You step through the node until you reach the players, checking for collisions along the
; way.
(define (no-collisions? pl quad)
  ; [Listof Entity] -> Boolean
  (define (collisions-between? loe)
    (andmap (λ (an-entity) (not (collides? pl an-entity)))
            (filter-not player? loe)))

  ; [Listof Node] -> Boolean
  (define (collisions-in? children)
    (andmap (λ (child) (no-collisions? pl child))
            children))

  (if quad
      (cond
        [(empty? (node-content quad))
         (collisions-in? (node-children quad))]
        [(zero? (vector-length (node-children quad)))
         (collisions-between? (node-content quad))]
        [else
         (and (collisions-between? (node-content quad))
              (collisions-in? (node-children quad)))])
      #t))


; Player Turret -> Turret
(define (update-turret tr pl)
  (define tpos (entity-coord tr))
  (define ppos (entity-coord pl))
  (define new-turret
    (struct-copy turret tr
                 [rotat (atan (- (posn-x tpos) (posn-x ppos))
                              (- (posn-y tpos) (posn-y ppos)))]))
  ; - IN -
  (if (zero? (turret-fire new-turret))
      (fire-projectile new-turret)
      (countdown new-turret)))

; Turret -> Turret
; This creates a projectile. The projectile is kept in the turret's struct until
; it's inserted into the quadtree, whereupon the counter is reset to the amount
; of time until firing.
(define (fire-projectile tr)
  (match tr
    [(turret coord width height rotation firing)
     (turret coord width height rotation
             (projectile coord (image-width MISSILE) (image-height MISSILE)
                         MSPEED rotation))]))

; Turret -> Turret
(define (countdown tr)
  (define count (turret-fire tr))
  (struct-copy turret tr
               [fire (sub1 count)]))

; Pvec -> Pvec
; rotates the quadrant in a cartesian plane a vector is in
; in accordance with how racket interprets negative and positive movement.
(define (rotate-quad vec)
  (match vec
    [(pvec x y) (pvec x (- y))]))

; Game -> Boolean
; the game is over if the player is hit by a projectile or reaches the end-zone.
(define (game-over? g)
  #f)

; -------------------------------------------------------------------------------------
#| RENDERING |#
; -------------------------------------------------------------------------------------
; Game Images -> Image Idea: rendering via pattern-matching. Take the quadtree,
; and use pattern-matching to recursively step through it, rendering all content along the way.
(define (render-game g img)
  (render-ship (game-p g)
               (render-turret (game-t g) (game-p g)
                              (render-obst (game-o g)
                                           (draw-quad (game-q g) WIDTH
                                                      (render-bg (game-p g) BACKG))))))
(define (render-g g img0)
  (define quad (game-q g))
  ; - IN -
  (render-tree quad img0))

; Node Image -> Image
(define (render-tree tree img)
  (cond
    [(and (zero-content? tree) (zero-children? tree))
     img]
    [(and (zero-content? tree) (not (zero-children? tree)))
     (render-child* (node-children tree) img)]
    [(and (not (zero-content? tree)) (not (zero-children? tree)))
     (define new-img (render-content (node-content tree) img))
     (render-child* (node-children tree) new-img)]
    [(and (not (zero-content? tree)) (zero-children? tree))
     (render-content (node-content tree) img)]))

; Node -> Boolean
(define (zero-children? q)
  (zero? (vector-length (node-children q))))

; Node -> Boolean
(define (zero-content? q)
  (empty? (node-content q)))

; [Listof Entity] Image -> Image
; renders a list of entities onto an image
(define (render-content things base-img)
  (foldl (λ (something img)
           (render-entity something img))
         base-img
         things))

; Entity Image -> Image
; renders a single entity onto another image
(define (render-entity thing img)
  (cond
    [(player? thing) (render-ship thing img)]
    [(turret? thing) (render-turret thing img)] ; needs player!
    [(projectile? thing) (render-one-obst thing img)]
    [else (render-one-obst thing img)]))

; Entity Image -> Image
(define (render-one-obst thing img)
  (match thing
    [(entity (posn x y) width height)
     (place-image (rectangle width height 'solid 'red) x y img)]))

; [Vectorof Child] Image -> Image
(define (render-child* child* img)
  (for/fold ([base img])
            ([a-child (in-vector child*)])
    (render-tree a-child base)))

; Node Image -> Image
(define (draw-quad node0 bounds0 img0)
  ; [Vectorof Node] Image -> Image
  (define (draw-children child* bounds im)
    (match child*
      ['#() im]
      [(vector a-child ...)
       (foldr (λ (sqr-to-draw rst) (draw-quad/a sqr-to-draw (/ bounds 2) rst))
              im
              a-child)]))

  ; Node Number Image -> Image
  (define (draw-quad/a node bounds img)
    (draw-children (node-children node) bounds
                   (place-image (square bounds 'outline 'red)
                                (+ (/ bounds 2) (posn-x (node-coord node)))
                                (+ (/ bounds 2) (posn-y (node-coord node)))
                                img)))
  ; - IN -
  (draw-quad/a node0 bounds0 img0))



; Player Image -> Image
(define (render-ship pl im)
  (place-image
   (rotate (rationalize
            (* (radians->degrees TURN-RATE) (player-turns pl))
            .5)
           SHIP)
   (posn-x (entity-coord pl))
   (posn-y (entity-coord pl))
   im))

; Turret Image Player -> Image
(define (render-turret tr pl im)
  (define crds (entity-coord tr))
  (place-image (track tr TURRET)
               (posn-x crds) (posn-y crds)
               im))

; Turret Image Posn -> Image
(define (track tr im)
  (rotate (radians->degrees (turret-rotat tr))
          im))

; [Listof Entity] Image -> Image
(define (render-obst obstacles im)
  (foldl (λ (an-entity img)
           (match-define (entity (posn x y) width height) an-entity)
           (place-image (rectangle width height 'solid 'red) x y img))
         im
         obstacles))

; Player Image -> Image
(define (render-bg pl im)
  (define coords (entity-coord pl))
  #| (scroll im
  (posn-x coords)
  (posn-y coords)
  (/ (difference (posn-x coords) 360)
  360)
  (/ (difference (posn-y coords) 360)
  360))) |#
  ;(crop INIT_X INIT_Y WIDTH HEIGHT im))
  im)

; Number Number -> Number
; consumes two numbers and produces the difference between them
(define (difference n m)
  (abs (- n m)))

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


; Posn Posn -> Number
; determines the distance between two cartesian points. Using this instead
; of taking the "straight" difference between x/y and 360 leads to some odd
; results corresponding to sine/cosine motions around the circle.
(define (distance point origin)
  (sqrt (+ (sqr (- (posn-x point) (posn-x origin)))
           (sqr (- (posn-x point) (posn-y origin))))))
;--------------------------------------------------------------------------------
#| DEBUGGING |#
;--------------------------------------------------------------------------------
; Player Image -> Image
(define (render-ui pl im)
  (overlay/align 'center 'bottom
                 (text (number->string (player-magni pl)) 20 'black)
                 im))

; [Listof Number] -> Image
; takes in a list of information, transforms it into strings, then appends
; labels to it, then flattens it into a single string and renders it.
(define (render-info info*)
  (define lo-str (map number->string info*))
  (define label+info (map (λ (label info) (string-append label " " info " "))
                          (list "cur-x:" "cur-y:")
                          lo-str))
  (text (apply string-append label+info) 20 'black))

; Number X [Listof [X -> Y]] -> [Listof Benchmarks]
; time the running of multiple functions over a base input
; over a period of time in ticks.
(define (benchmark-funcs tm input . funcs)
  (for ([func funcs])
    (displayln func)
    (time (for ([i (in-range tm)])
            (func input)))))

#;(define start-game
    (thread start))
