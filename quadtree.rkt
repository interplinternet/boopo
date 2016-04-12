#lang racket
(require 2htdp/image 2htdp/universe)
;-------------------------------------------------------------------------------------
#| DATA |#
;-------------------------------------------------------------------------------------
(struct posn (x y) #:transparent)

(struct node (coord children) #:transparent)
; A Node is (node (posn #(node node node node)))

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
(define root (node (posn 0 0) '())) ; a node with no entities
(define root1 (node (posn 0 0) '(flying-ship)))
(define root2 (node (posn 0 0) MAX-THINGS)) ;max-nodes, split!
#;(define root3 (node (posn 0 0) (list (node (posn 0 0) ...)
                                     (node (posn 0 2) ...)
                                     (node (posn 2 0) ...)
                                     (node (posn 2 2) ...))))
;-------------------------------------------------------------------------------------
#| LOGIC |#
;-------------------------------------------------------------------------------------
; for easy prototyping, we use lists to represent the tree.
; Number Number Number Number -> Quadtree
(define (make-empty-tree x y dimension depth)
  (cond
    [(zero? depth) '()]
    [else (define mid (/ dimension 2))
          (define new-squares (make-squares x y mid))
          (foldr (λ (a-sqr rst)
                   (list* a-sqr
                          (make-empty-tree
                           (first a-sqr) (second a-sqr) mid (sub1 depth))
                          rst))
                 '()
          new-squares)]))
          #| (for/fold ([acc   '()])
                    ([a-sqr new-squares])
            (list* a-sqr
                   (make-empty-tree (first a-sqr) (second a-sqr) mid (sub1 depth))
                   acc))])) |#

; Number Number Number -> [Listof Number Number]
(define (make-squares x y dimension)
  ; isn't the dimension half of the largest value?
  ; i.e., in (0, 1) => .5, (2, 3) -> 2
  (list (list x y)
        (list (+ x dimension) y)
        (list x (+ y dimension))
        (list (+ x dimension) (+ y dimension))))

(define (build-lazy n proc)
  (build-list n (λ (i) (delay (proc i)))))

(define possible-points
  (combinations (build-list 4 identity) 2))

(define bound-4-tree
  (cons '(0 0) (combinations (build-list 4 identity) 2)))

