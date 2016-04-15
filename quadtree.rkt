#lang racket
(require 2htdp/image 2htdp/universe)
;-------------------------------------------------------------------------------------
#| DATA |#
;-------------------------------------------------------------------------------------
(struct posn (x y) #:transparent)

(struct node (coord content children) #:transparent)
; A Node is (node (posn Number Number) Any [List Node Node Node Node])

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
(define root (node (posn 0 0) '() '())) ; a node with no entities
(define root1 (node (posn 0 0) '(flying-ship) '()))
(define root2 (node (posn 0 0) MAX-THINGS '())) ;max-nodes, split!
#;(define root3 (node (posn 0 0) (list (node (posn 0 0) ...)
                                     (node (posn 0 2) ...)
                                     (node (posn 2 0) ...)
                                     (node (posn 2 2) ...))))


;-------------------------------------------------------------------------------------
#| LOGIC |#
;-------------------------------------------------------------------------------------

;----------POSNS----------
; [Listof Posn] -> Boolean
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

;----------QUADTREE----------
; for easy prototyping, we use lists to represent the tree. The nodes contain
; nothing except for their coordinates and their children.

; If we use vectors, we can use the vector's index to represent the quadrants
; of the screen or area.
; 0 = Upper-left, 1 = Upper-right, 2 = Lower-left, 3 = Lower-right.

; Node := [List [List Number Number] [Listof Node]]
; Posn Number Number -> Quadtree
; Depth represents levels past the root node. A depth of 0 is just the root node.
#| We could make only a single node, and then split when there are objects or
 make the entire empty tree at once and then pass it around and insert/retrieve
 objects as needed by copying the data structure.

Maybe we could combine this with the root-and-split approach?
Each level is wrapped in a delay, then evaluated as necessary. Every time we
need to recreate the tree (as entities move), we copy every element in the tree and
update only those which have been forced already, because unforced levels
could not have been changed.

i.e., we traverse the tree and update it as we move along until we hit a delayed evaluation
and then we copy it over. We "split" the tree by forcing the next level. Branches which
no longer contain entities are pruned?|#
(define (make-empty-tree coords dimension depth)
  ; [Listof [List Number Number]] -> Node
  ; this could be inlined, but having it separate makes it a little easier
  ; to understand what it does.
  (define (make-sub-trees mid lo-sqr)
    (filter-map (λ (a-sqr) (and (not (empty? a-sqr))
                                (make-empty-tree a-sqr mid (sub1 depth))))
                lo-sqr))
  ; - IN -
  (cond
    [(zero? depth) (node coords '() '())]
    [else (define mid (/ dimension 2))
          (define new-squares (make-squares (posn-x coords) (posn-y coords) mid))
          (define subtrees (make-sub-trees mid new-squares))
          ; - IN -
    (node coords '() subtrees)]))

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

(define empty-tree (make-empty-tree (posn 0 0) 2 2))
(define root3 (node (posn 0 0) '() '()))

; Node Number -> Node
; splits a node with dimension "bounds" into a node with 4 subnodes
(define (split a-node bounds)
  (define base-coords (node-coord a-node))
  (define new-quads (make-squares (posn-x base-coords) (posn-y base-coords)
                                  (/ bounds 2)))
  ; - IN -
  (struct-copy node a-node
               [children (map (λ (child) (node child '() '())) new-quads)]))
#|
Entity := Ship
        | Turret
        | Projectile
|#
; Node Number Entity -> [Maybe Number]
; consumes a tree, the dimension at that level,
; and returns the index of the quadrant it belongs in.
; Assumes an already split node. #f if it cannot fit.
; From left->right, top->bottom, 0->3
(define (get-index tree bounds location)
  (define quadrants
    (if (empty? (node-children tree))
        (node-children (split tree bounds))
        (node-children tree)))
  ; [Listof Node] Number -> Node
  ; the accumulator represents the index of the node being investigated.
  (define (index/a subquads index)
    (cond
      [(empty? subquads) #f]
      [else
       (if (fits? (first subquads) location (/ bounds 2))
           index
           (index/a (rest subquads) (add1 index)))]))
  ; - IN -
  (index/a quadrants 0))

; Entity Node Number -> Boolean
(define (fits? child location bounds)
  (define coords (node-coord child))
  ; currently, this only compares the central coordinates of the entity
  ; and doesn't take it's dimensions into account.
  ; For example, given a child-coord of (posn 0 0), bounds of 1, a ship centered at
  ; (posn 1 1/2), the ship is placed at index 0 in the upper left.
  ; It occupies the edge between 0 and 1 index.
  (posn< coords location (posn (+ (posn-x coords) bounds)
                               (+ (posn-y coords) bounds))))


; Node Number Entity -> Node
; Problem: insert an entity into the appropriate subnode.
; Determine the quadrant index it belongs to. If it doesn't belong to any quadrant,
; place in the root node. If the indexed node it belongs to doesn't have room, split
; and place in the appropriately indexed subnode.
; Trivial: All subnodes are empty. Insert into the indexed node.
; Generation: The subnode is full, split and recurse.
; Termination: Terminates by splitting a node, which must be empty by definition,
; and then inserting the entity into it.

; Caveat: If the entity does not fit into any particular quadrant, the subnodes are still
; created but the entity still sits at the root node. This complicates retrieval,
; b/c now we have two potential locations for an entity.
(define (insert-node tree bounds entity)
  (define child* (node-children tree))
  (define index (get-index tree bounds entity))

  ; [Listof Node] -> [Listof Node]
  (define (update-child child entity)
    (cond
      [(< (length (node-content child)) MAX)
       (node (node-coord child)
             (cons entity (node-content child))
             (node-children child))]
      [else
       (insert-node child (/ bounds 2) entity)]))

  ; - IN -
  (cond
    [(false? index) (node (node-coord tree) (cons entity (node-content tree)) child*)]
    [(empty? child*)
     (define new-tree (split tree bounds))
     (insert-node new-tree bounds entity)]
    [(cons? child*)
     (struct-copy
      node tree
      [children (list-update child* index (λ (child) (update-child child entity)))])]))

; Entity Number Node -> [Listof Entity]
; should retrieve all entities an entity could collide with.
; To do so, it gets the index of the entity, then returns all entities which are
; above it in the tree. We need to determine where the entity exists in the tree.4
; Possible: Traverse the tree while accumulating the entities in each level until
;  you reach the entity searched for, then return all entities.
(define (retrieve-node entity bounds tree)
  (define index (get-index tree bounds entity)) ; how do we determine bounds?
  (if index
      (append (node-content (list-ref (node-children tree) index))
              (retrieve entity (/ bounds 2) (list-ref (node-children tree) index)))
      '())) ; or (node-content tree)
