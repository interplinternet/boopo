#lang racket
(provide (struct-out entity) (struct-out node) (struct-out posn)
         (struct-out entity) VROOT
         depth-set! bounds-set! max-set!
         retrieve-node insert-node)
(require racket/trace)
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

; Number -> Void
; provides a way to alter these constants from outside the module.
(define (depth-set! n)
  (set! DEPTH n))

; Number -> Void
(define (bounds-set! n)
  (set! BOUNDS n))

; Number -> Void
(define (max-set! n)
  (set! MAX n))
;-------------------------------------------------------------------------------------
#| EXAMPLES |#
;-------------------------------------------------------------------------------------
;(define MAX-THINGS '(flying-ship other-ship uhh some-thing))
;(define ONE-THING '(flying-ship))
;(define TWO-THINGS '(flying-ship other-ship))
(define ROOT (node (posn 0 0) '() '())) ; a node with no entities
(define VROOT (node (posn 0 0) '() #()))
;(define root2 (node (posn 0 0) MAX-THINGS '())) ;max-nodes, split!
#;(define root3 (node (posn 0 0) (list (node (posn 0 0) ...)
                                     (node (posn 0 2) ...)
                                     (node (posn 2 0) ...)
                                     (node (posn 2 2) ...))))
(define ex-ent (entity (posn 3/2 1/2) 1 1))


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
(define (in-bounds? quad-coords thing bounds)
  (define ent-coords (entity-coord thing))
  (and (<= (posn-x quad-coords) (posn-x ent-coords)
          (+ (posn-x ent-coords) (entity-width thing))
          (posn-x bounds))
       (>= (posn-y bounds)
          (+ (posn-y ent-coords) (entity-height thing))
          (posn-y ent-coords) (posn-y quad-coords))))

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

(define ex-squares (make-squares 0 0 2))

; Node Number -> Node
; splits a node with dimension "bounds" into a node with 4 subnodes
(define (split a-node [bounds BOUNDS])
  (define base-coords (node-coord a-node))
  (define new-quads (make-squares (posn-x base-coords) (posn-y base-coords)
                                  (/ bounds 2)))
  ; - IN -
  (struct-copy node a-node
               ;[children (map (λ (coords) (node coords '() '())) new-quads)]))
               [children (for/vector #:length 4
                                     ([coord (in-list new-quads)])
                           (node coord '() #()))]))


; Node Entity Number -> [Maybe Number]
; consumes a tree, the dimension at that level,
; and returns the index of the quadrant it belongs in.
; Assumes an already split node. #f if it cannot fit.
; From left->right, top->bottom, 0->3
(define (get-index tree an-entity [bounds BOUNDS])
  (define quadrants
    (if (zero? (vector-length (node-children tree)))
        (node-children (split tree bounds))
        (node-children tree)))

  (define half-bounds (/ bounds 2))
  ; [Listof Node] Number -> Node
  ; the accumulator represents the index of the node being investigated.
  (define (index/a subquads)
    (for/first ([(quad index) (in-indexed (in-vector subquads))]
                #:when (fits? quad an-entity half-bounds))
      index))
  (index/a quadrants))

; Node Entity Number -> Boolean
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
  (define index (get-index tree entity bounds))

  ; Node -> Node
  ; consumes a child node and an entity to insert into it. If there's room,
  ; insert it. If not, split the child and recurse.
  (define (update-child child entity)
    (cond
      [(< (length (node-content child)) MAX)
       (struct-copy node child
                    [content (cons entity (node-content child))])]
      [else
       (insert-node child entity (/ bounds 2))]))

  ; - IN -
  (cond
    [(false? index)
     (node (node-coord tree) (cons entity (node-content tree)) child*)]
    [(zero? (vector-length child*))
     (define new-tree (split tree bounds))
     (insert-node new-tree entity bounds)]
    [else
     (struct-copy
      node tree
      [children
       (vector-update child* index (λ (child)
                                     (update-child child entity)))])]))

; Vector Number [X->Y] -> Vector
; functionally updates a vector, where the value at the index is replaced
; by the result of calling that function on that value.
(define (vector-update vec i func)
  (for/vector ([(v ref) (in-indexed (in-vector vec))])
    (if (= ref i) (func v) v)))

; Entity Number Node -> [Listof Entity]
; should retrieve all entities an entity could collide with.
; Traverse the tree while accumulating the entities in each level until
;  you pass the entity searched for, then return all entities in that node.
(define (retrieve-node entity tree [bounds BOUNDS])
  (define index (get-index tree entity bounds))
  (if index
      (retrieve-node entity (vector-ref (node-children tree) index) (/ bounds 2))
      (node-content tree)))
