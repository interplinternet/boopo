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
(define (make-empty-tree coords dimension depth)
  ; [Listof [List Number Number]] -> Node
  ; this could be inlined, but having it separate makes it a little easier
  ; to understand what it does.
  (define (make-sub-trees mid lo-sqr)
    (map (λ (a-sqr) (make-empty-tree a-sqr mid (sub1 depth)))
         lo-sqr))
  ; - IN -
  (cond
    [(zero? depth) (node coords '() '())]
    [else (define mid (/ dimension 2))
          (define new-squares (make-squares (posn-x coords) (posn-y coords) mid))
          (define subtrees (filter (λ (i) (not (empty? i)))
                                   (make-sub-trees mid new-squares)))
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

; Posn Node -> [Maybe Any]
; Question: Do we retrieve the first node in which the coordinate exists,
; or the last? (i.e., the largest possible area or the smallest? If the coord is 0,0
; then we could be retrieving the entire screen, OR an infinitesimaly small part
; in the upper left corner). We might need to take dimension into account as well.
(define (retrieve-node coords tree)
  ; [Listof Node] -> Any
  (define (search-in subtree)
    (cond
      [(empty? subtree) #f]
      [else
       (define candidate (retrieve-node coords (first subtree)))
       (if candidate candidate (search-in (rest subtree)))]))
  (cond
    [(posn=? coords (node-coord tree)) (node-content tree)]
    [(cons? (node-children tree))
     ;(ormap (λ (child) (retrieve-node coords child)) (node-children tree))]))
     ; depth-first searching for nodes
     (search-in (node-children tree))]
    [(empty? (node-children tree)) #f]))

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

; Posn Posn -> Boolean
(define (posn=? p1 p2)
  (and (= (posn-x p1) (posn-x p2))
       (= (posn-y p1) (posn-y p2))))

(define (build-lazy n proc)
  (build-list n (λ (i) (delay (proc i)))))
