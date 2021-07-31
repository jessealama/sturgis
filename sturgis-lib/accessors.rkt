#lang typed/racket/base

(require "types.rkt")

(provide attributes
         base-uri
         children
         document-uri
         is-id?
         is-idrefs?
         namespace-nodes
         nilled?
         node-kind
         node-name
         parent
         string-value
         type-name
         typed-value
         unparsed-entity-public-id
         unparsed-entity-system-id)

(: attributes (-> XDMNode
                  (Listof attribute-node)))
(define (attributes aNode)
  (list))

(: base-uri (-> XDMNode
                (Option String)))
(define (base-uri aNode)
  #f)

(: children (-> XDMNode
                (Listof node)))
(define (children aNode)
  (node-children aNode))

(: document-uri (-> XDMNode
                    (Option String)))
(define (document-uri aNode)
  #f)

(: is-id? (-> XDMNode Boolean))
(define (is-id? aNode)
  #f)

(: is-idrefs? (-> XDMNode Boolean))
(define (is-idrefs? aNode)
  #f)

(: namespace-nodes (-> XDMNode
                       (Listof namespace-node)))
(define (namespace-nodes aNode)
  (list))

(: nilled? (-> XDMNode Boolean))
(define (nilled? aNode)
  #f)

(: node-kind (-> XDMNode
                 (U 'attribute
                    'comment
                    'document
                    'element
                    'namespace
                    'processing-instruction
                    'text)))
(define (node-kind aNode)
  'text)

(: node-name (-> XDMNode
                 String))
(define (node-name aNode)
  "")

(: parent (-> XDMNode
              (Option node)))
(define (parent aNode)
  (node-parent aNode))

(: text-content->string (-> (Listof (U String Symbol Exact-Nonnegative-Integer))
                            String))
(define (text-content->string pieces)
  (cond [(null? pieces)
         ""]
        [else
         (define p (car pieces))
         (define s
           (cond [(string? p)
                  p]
                 [(symbol? p)
                  "entity!"]
                 [else
                  (format "~a" (integer->char (car pieces)))]))
         (string-append s (text-content->string (cdr pieces)))]))

(: string-value (-> XDMNode
                    String))
(define (string-value aNode)
  (cond [(text-node? aNode)
         (text-content->string (text-node-content aNode))]
        [(attribute-node? aNode)
         (attribute-node-value aNode)]
        [(element-node? aNode)
         (apply string-append
                (map string-value
                     (filter text-node? (node-children aNode))))]
        [else
         (error (format "What is the string value of ~a" aNode))]))

(: type-name (-> XDMNode
                 (Option String)))
(define (type-name aNode)
  #f)

(: typed-value (-> XDMNode
                   (Listof AtomicXDMItem)))
(define (typed-value aNode)
  (error (format "What is the typed value of ~a" aNode)))

(: unparsed-entity-public-id (-> XDMNode
                                 String
                                 (Option String)))
(define (unparsed-entity-public-id aNode entityName)
  #f)

(: unparsed-entity-system-id (-> XDMNode
                                 String
                                 (Option String)))
(define (unparsed-entity-system-id aNode entityName)
  #f)
