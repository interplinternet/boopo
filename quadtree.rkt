#lang racket
(provide entity node posn
         retrieve-node insert-node)
;-------------------------------------------------------------------------------------
#| DATA |#
;-------------------------------------------------------------------------------------
(struct posn (x y) #:transparent) ; this will also be available in the main file
(struct node (coord content children) #:transparent)
; A Node is (node (posn Number Number) Any [List Node Node Node Node])

(struct entity (coord width height) #:transparent)
; An entity is (entity (posn Number Number) Number Number)
#|
Entity := Ship
        | Turret
        | Projectile
|#
;-------------------------------------------------------------------------------------
#| CONSTANTS |#
;-------------------------------------------------------------------------------------
(define DEPTH 3)
(define BOUNDS 4) ; width and height, since we use a square
(define MAX 3)
; no more than thre objects per node.
; reason: the game only has a turret, a ship, and a projectile.
;-------------------------------------------------------------------------------------
#| EXAMPLES |#
;-------------------------------------------------------------------------------------
(define MAX-THINGS '(flying-ship other-ship uhh some-thing))
(define ONE-THING '(flying-ship))
(define TWO-THINGS '(flying-ship other-ship))
(define ROOT (node (posn 0 0) '() '())) ; a node with no entities
;(define root2 (node (posn 0 0) MAX-THINGS '())) ;max-nodes, split!
#;(define root3 (node (posn 0 0) (list (node (posn 0 0) ...)
                                     (node (posn 0 2) ...)
                                     (node (posn 2 0) ...)
                                     (node (posn 2 2) ...))))


;-------------------------------------------------------------------------------------
#| LOGIC |#
;-------------------------------------------------------------------------------------
;----------POSNS----------
; [X -> Y] [Listof Posn] -> Boolean
; gives false if the ship is "on the line", b/c it must does not fit in any particular quad
(define (posn-oper oper coords)
  (and (apply oper (map posn-x coords))
       (apply oper (map posn-y coords))))

(define (posn=? . posns)
  (posn-oper = posns))

(define (posn< . coords)
  (posn-oper < coords))

(define (posn> . coords)
  (posn-oper > coords))

; Posn Entity Posn -> Boolean
(define (in-bounds? coords thing bounds)
  (and (< (posn-x coords) (posn-x (entity-coord thing))
          (+ (posn-x (entity-coord thing)) (entity-width thing))
          (posn-x bounds))
       (< (posn-y coords) (posn-y (entity-coord thing))
          (+ (posn-y (entity-coord thing)) (entity-height thing))
          (posn-y bounds))))

;----------QUADTREE----------
; for easy prototyping, we use lists to represent the tree.
; If we use vectors, we can use the vector's index to represent the quadrants
; of the screen or area.
; 0 = Upper-left, 1 = Upper-right, 2 = Lower-left, 3 = Lower-right.

; Number Number Number -> [Listof Number Number]
(define (make-squares x y dimension)
  ; isn't the dimension half of the largest value?
  ; i.e., in (0, 1) => .5, (2, 3) -> 2
  ; this function feels very "off". Isn't there a way to traverse the list
  ; and create the values?
  ; Steps: Add dimension to none, then first, then second, then both values.
  ; (x y) (fx y) (x fy) (fx fy), looks kind of like FOIL-distribution.
  (list (posn x y)
        (posn (+ x dimension) y)
        (posn x (+ y dimension))
        (posn (+ x dimension) (+ y dimension))))

; Node Number -> Node
; splits a node with dimension "bounds" into a node with 4 subnodes
(define (split a-node [bounds BOUNDS])
  (define base-coords (node-coord a-node))
  (define new-quads (make-squares (posn-x base-coords) (posn-y base-coords)
                                  (/ bounds 2)))
  ; - IN -
  (struct-copy node a-node
               [children (map (λ (child) (node child '() '())) new-quads)]))

; Node Number Entity -> [Maybe Number]
; consumes a tree, the dimension at that level,
; and returns the index of the quadrant it belongs in.
; Assumes an already split node. #f if it cannot fit.
; From left->right, top->bottom, 0->3
(define (get-index tree location [bounds BOUNDS])
  (define quadrants
    (if (empty? (node-children tree))
        (node-children (split tree bounds))
        (node-children tree)))

  ; [Listof Node] Number -> Node
  ; the accumulator represents the index of the node being investigated.
  (define (index/a subquads index)
    (cond
      [(empty? subquads) #f]
      [(fits? (first subquads) location (/ bounds 2)) index]
      [else (index/a (rest subquads) (add1 index))]))
      ; - IN -
  (index/a quadrants 0))

; Entity Node Number -> Boolean
(define (fits? child thing [bounds BOUNDS])
  (define coords (node-coord child))
  (in-bounds? coords
              thing
              (posn (+ (posn-x coords) bounds)
                    (+ (posn-y coords) bounds))))


; Node Number Entity -> Node
; Problem: insert an entity into the appropriate subnode.
; Determine the quadrant index it belongs to. If it doesn't fit in any quadrant,
; place in the root node. If the indexed node it belongs to doesn't have room, split
; and place in the appropriately indexed subnode.
; Trivial: All subnodes are empty. Insert into the indexed node.
; Generation: The subnode is full, split and recurse.
; Termination: Terminates by splitting a node, which must create empty nodes,
; and then inserting the entity into it.
(define (insert-node tree entity [bounds BOUNDS])
  (define child* (node-children tree))
  (define index (get-index tree bounds entity))

  ; [Listof Node] -> [Listof Node]
  (define (update-child child entity)
    (cond
      [(< (length (node-content child)) MAX)
       (struct-copy node child
                    [children (cons entity (node-content child))])]
      [else
       (insert-node child entity (/ bounds 2))]))

  ; - IN -
  (cond
    [(false? index)
     (node (node-coord tree) (cons entity (node-content tree)) child*)]
    [(empty? child*)
     (define new-tree (split tree bounds))
     (insert-node new-tree entity bounds)]
    [(cons? child*)
     (struct-copy
      node tree
      [children (list-update child* index (λ (child) (update-child child entity)))])]))

; Entity Number Node -> [Listof Entity]
; should retrieve all entities an entity could collide with.
; Traverse the tree while accumulating the entities in each level until
;  you pass the entity searched for, then return all entities in that node.
(define (retrieve-node entity tree [bounds BOUNDS])
  (define index (get-index tree bounds entity))
  (if index
      (retrieve-node entity (list-ref (node-children tree) index) (/ bounds 2))
      (node-content tree)))
