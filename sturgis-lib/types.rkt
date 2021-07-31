#lang typed/racket/base

(provide (struct-out node)
         (struct-out attribute-node)
         (struct-out text-node)
         (struct-out namespace-node)
         (struct-out processing-instruction-node)
         (struct-out comment-node)
         (struct-out document-node)
         (struct-out element-node)
         (struct-out xdm-array)
         (struct-out xdm-map)

         XDMNode
         xdm-node?
         AtomicXDMItem
         atomic-xdm-item?
         XDMItem
         xdm-item?
         XDMSequence
         xdm-sequence?
         XDMValue

         XExpr
         XExprAttribute
         xexpr-attribute?

         XMLThing
         AxisSymbol)

(require (prefix-in xml: "xml.rkt"))

(struct node
  ([parent : (Option XDMNode)]
   [children : (Listof XDMNode)])
  #:mutable)

(struct attribute-node node
  ([name : String]
   [value : String]))

(struct text-node node
  ([content : (Listof (U String Symbol Exact-Nonnegative-Integer))]))

(struct namespace-node node
  ([prefix : (Option String)]
   [uri : String]))

(struct processing-instruction-node node
  ([target : String]
   [instruction : String]))

(struct comment-node node
  ([content : String]))

(struct document-node node
  ([uri : (Option String)]))

(struct element-node node
  ([name : String]
   [attributes : (Listof attribute-node)])
  #:mutable)

(define-type XDMNode (U attribute-node
                        text-node
                        namespace-node
                        processing-instruction-node
                        comment-node
                        document-node
                        element-node))

(define-predicate xdm-node? XDMNode)

(struct xdm-array
  ([entries : (Listof XDMValue)]))

(struct xdm-map
  ([data : (Immutable-HashTable Symbol XDMValue)]))

(define-type AtomicXDMItem (U String
                              Number
                              Boolean))

(define-predicate atomic-xdm-item? AtomicXDMItem)

(define-type XDMItem (U AtomicXDMItem
                        xdm-map
                        xdm-array
                        attribute-node
                        text-node
                        namespace-node
                        processing-instruction-node
                        comment-node
                        document-node
                        element-node))

(define-predicate xdm-item? XDMItem)

(define-type XDMSequence (Listof XDMItem))

(define-predicate xdm-sequence? XDMSequence)

(define-type XDMValue (U XDMItem XDMSequence))

(define-type XExprAttribute (List Symbol String))

(define-predicate xexpr-attribute? XExprAttribute)

(define-type XExpr
  (U String
     Symbol
     Char
     (Pairof Symbol
             (U (Listof XExpr)
                (Pairof (Listof XExprAttribute)
                        (Listof XExpr))))))

(define-type XMLThing (U xml:comment
                         xml:p-i
                         xml:document
                         xml:element
                         xml:attribute
                         xml:entity
                         xml:cdata
                         xml:pcdata))

(define-type AxisSymbol (U 'ancestor
                           'ancestor-or-self
                           'attribute
                           'child
                           'descendant
                           'descendant-or-self
                           'following
                           'following-sibling
                           'namespace
                           'parent
                           'preceding
                           'preceding-sibling
                           'self))
