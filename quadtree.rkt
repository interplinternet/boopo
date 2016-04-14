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
 objects as needed by copying the data structure. |#
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
  (define quadrants (node-children tree))
  ; [Listof Node] Number -> Node
  ; the accumulator represents the index of the node being investigated.
  (define (index/a subquads index)
    (cond
      [(empty? subquads) #f]
      [else
       (if (fits? location (first subquads) bounds)
           index
           (index/a (rest subquads) (add1 index)))]))
    ; - IN -
    (index/a quadrants 0))

; Entity Node Number -> Boolean
(define (fits? location child bounds)
  (define coords (node-coord child))
  ; currently, this only compares the central coordinates of the entity
  ; and doesn't take it's dimensions into account.
  ; For example, given a child-coord of (posn 0 0), bounds of 1, a ship centered at
  ; (posn 1 1/2), the ship is placed at index 0 in the upper left.
  ; It occupies the edge between 0 and 1 index.
  (posn< coords location (posn (+ (posn-x coords) bounds)
                                (+ (posn-y coords) bounds))))

; [Listof Posn] -> Boolean
; gives false if the ship is "on the line", b/c it must does not fit in any particular quad.
  (define (posn< . coords)
    (and (apply < (map posn-x coords))
         (apply < (map posn-y coords))))

; Node Number Entity -> Node
; Problem: insert an entity into the appropriate subnode.
; Determine the quadrant index it belongs to. If it doesn't belong to any quadrant,
; place in the root node. If the indexed node it belongs to doesn't have room, split
; and place in the appropriately indexed subnode.
; Trivial: All subnodes are empty. Insert into the indexed node.
; Generation: The subnode is full, split and recurse.
; Termination: Terminates by splitting a node, which must be empty by definition,
; and then inserting the entity into it.

; Caveat: If the entity does not fit into any particular quadrant, the subnodes are still created
; but the entity still sits at the root node. This complicates retrieval,
; b/c now we have two potential locations for an entity.
(define (insert-node tree bounds entity)
  (define child* (node-children tree))

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
    ; maybe we should make the index the primary data here, to avoid needless splitting.
    [(empty? child*) ; a node can have no children yet
     (define new-tree (split tree bounds))
     (insert-node new-tree bounds entity)]
    [(cons? child*)
     (define index (get-index tree bounds entity))
     (cond
       [(false? index)
        (node (node-coord tree)
              (cons entity (node-content tree))
              child*)]
       [else
        (struct-copy node tree ; wow this is messy!
                     [children
                      (list-update child* index
                                (λ (child) (update-child child entity)))])])]))
; Posn Node -> [Maybe Any]
; Question: Do we retrieve the first node in which the coordinate exists,
; or the last? (i.e., the largest possible area or the smallest? If the coord is 0,0
; then we could be retrieving the entire screen, OR an infinitesimaly small part
; in the upper left corner). Then we might need to take dimension into account as well
; Dimension is related to depth: Where max bounds is "B" and depth is "n", B^1/n.
; Answer: We retrieve the last possible node. Now we don't need to worry about
; dimension, as we only split the tree to put new objects in.
(define (retrieve-node coords tree)
  ; [Listof Node] -> Any
  (define (search-in subtree)
    (cond
      [(empty? subtree) #f]
      [else
       (define candidate (retrieve-node coords (first subtree)))
       (if candidate candidate (search-in (rest subtree)))]))
  ; - IN -
  (cond
    [(posn=? coords (node-coord tree)) (node-content tree)]
    [(cons? (node-children tree))
     ;(ormap (λ (child) (retrieve-node coords child)) (node-children tree))]))
     ; depth-first searching for nodes
     (search-in (node-children tree))]
    [(empty? (node-children tree)) #f]))
#|
; Content Posn Node -> Node
; As a prototype, it's fine for this function to assume that we already have the coords
; of the appropriate node. See above for problems.
(define (insert-node entity coords tree)
  (define (sub-insert subtree)
    (map (λ (child) (insert-node entity coords child)) subtree))
  ; - IN -
  (cond
    [(posn=? coords (node-coord tree))
     (struct-copy node tree
                  [content entity])]
    [(cons? (node-children tree))
     (node (node-coord tree) (node-content tree)
           (sub-insert (node-children tree)))]
    [else tree]))
|#
; Posn Posn -> Boolean
(define (posn=? p1 p2)
  (and (= (posn-x p1) (posn-x p2))
       (= (posn-y p1) (posn-y p2))))
