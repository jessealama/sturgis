#lang typed/racket/base

(provide ancestor
         ancestor-or-self
         attribute
         child
         descendant
         descendant-or-self
         following
         following-sibling
         namespace
         parent
         preceding
         preceding-sibling
         self)

(require racket/list
         "types.rkt"
         "accessors.rkt")

(: ancestor : (-> XDMNode
                  (Listof XDMNode)))
(define (ancestor aNode)
  (define p (parent aNode))
  (cond [(node? p)
         (cons p (ancestor p))]
        [else (list)]))

(: ancestor-or-self (-> XDMNode
                        (Listof XDMNode)))
(define (ancestor-or-self aNode)
  (cons aNode (ancestor aNode)))

(: attribute (-> XDMNode
                 (Listof XDMNode)))
(define (attribute aNode)
  (cond [(element-node? aNode)
         (element-node-attributes aNode)]
        [else
         (list)]))

(: child (-> XDMNode
             (Listof XDMNode)))
(define (child aNode)
  (node-children aNode))

(: descendant (-> XDMNode
                  (Listof XDMNode)))
(define (descendant aNode)
  (define kids (node-children aNode))
  (cond [(eq? #f kids)
         (list)]
        [else
         (append kids
                 (apply append (map descendant kids)))]))

(: descendant-or-self (-> XDMNode
                          (Listof XDMNode)))
(define (descendant-or-self aNode)
  (cons aNode (descendant aNode)))

(: following-sibling (-> XDMNode
                         (Listof XDMNode)))
(define (following-sibling aNode)
  (define p (parent aNode))
  (cond [(null? p)
         (list)]
        [else
         (define kids (child (car p)))
         (define i (index-of kids aNode))
         (cond [(eq? #f i)
                (list)]
               [else
                (drop kids i)])]))

(: following (-> XDMNode
                 (Listof XDMNode)))
(define (following aNode)
  (define sibs (following-sibling aNode))
  (append sibs
          (apply append
                 (map descendant sibs))))

(: namespace (-> XDMNode
                 (Listof XDMNode)))
(define (namespace aNode)
  (list))

(: parent (-> XDMNode
              (Listof XDMNode)))
(define (parent aNode)
  (define p (node-parent aNode))
  (cond [(eq? #f p)
         (list)]
        [else
         (list p)]))

(: preceding-sibling (-> XDMNode
                         (Listof XDMNode)))
(define (preceding-sibling aNode)
  (define p (parent aNode))
  (cond [(null? p)
         (list)]
        [else
         (define mom (car p))
         (define kids (node-children mom))
         (cond [(eq? #f kids) ; shouldn't happen, but here you go:
                (list)]
               [else
                (define i (index-of kids aNode))
                (cond [(eq? #f i) ; shouldn't happen
                       (list)]
                      [else
                       (take kids i)])])]))

(: remove-duplicate-nodes (-> (Listof XDMNode)
                              (Listof XDMNode)))
(define (remove-duplicate-nodes nodes)
  (cond [(null? nodes)
         (list)]
        [(member (car nodes)
                 (cdr nodes))
         (remove-duplicate-nodes (cdr nodes))]
        [else
         (cons (car nodes)
               (remove-duplicate-nodes (cdr nodes)))]))

(: preceding (-> XDMNode
                 (Listof XDMNode)))
(define (preceding aNode)
  (define earlier-sibs (preceding-sibling aNode))
  (define result (remove-duplicate-nodes (append (append-map ancestor earlier-sibs)
                                                 earlier-sibs)))
  result)

(: self (-> XDMNode
            (Listof XDMNode)))
(define (self aNode)
  (list aNode))
