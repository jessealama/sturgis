#lang typed/racket/base

(provide xml->xdm
         xexpr->xdm)

(require (prefix-in xml: "xml.rkt")
         "types.rkt")

(: xml-attr->xdm-attr (-> xml:attribute
                          element-node
                          attribute-node))
(define (xml-attr->xdm-attr attr parent)
  (attribute-node parent
                         (list)
                         (symbol->string (xml:attribute-name attr))
                         (xml:attribute-value attr)))

; (struct defined in xml) (or node? #f) -> node?
(: xml->xdm (->* (XMLThing)
                 ((Option XDMNode))
                 XDMNode))
(define (xml->xdm x [parent #f])
  (cond [(xml:document? x)
         (define new-node (document-node parent (list) #f))
         (define kids (map (lambda ([n : XMLThing])
                             (xml->xdm n new-node))
                           (cons (xml:document-element x)
                                 (xml:document-misc x))))
         (set-node-children! new-node kids)
         new-node]
        [(xml:element? x)
         (define new-node (element-node parent
                                        (list) ; will be updated later
                                        (symbol->string (xml:element-name x))
                                        (list)))
         (define attrs (map (lambda ([a : xml:attribute])
                              (xml-attr->xdm-attr a new-node))
                            (xml:element-attributes x)))
         (define kids (map (lambda ([n : XMLThing])
                             (xml->xdm n new-node))
                           (xml:element-content x)))
         (set-node-children! new-node kids)
         (set-element-node-attributes! new-node attrs)
         new-node]
        [(xml:attribute? x)
         (attribute-node parent
                         (list)
                         (symbol->string (xml:attribute-name x))
                         (xml:attribute-value x))]
        [(xml:p-i? x)
         (processing-instruction-node parent
                                      (list)
                                      (symbol->string (xml:p-i-target-name x))
                                      (xml:p-i-instruction x))]
        [(xml:cdata? x)
         (text-node parent
                    (list)
                    (list (xml:cdata-string x)))]
        [(xml:pcdata? x)
         (text-node parent
                    (list)
                    (list (xml:pcdata-string x)))]
        [(xml:entity? x)
         (text-node parent
                    (list)
                    (list (xml:entity-text x)))]
        [(xml:comment? x)
         (comment-node parent
                       (list)
                       (xml:comment-text x))]))

(: xexpr-attrs (-> XExpr
                   (Listof XExprAttribute)))
(define (xexpr-attrs xe)
  (cond [(list? xe)
         (cond [(null? (cdr xe)) ; '(foo)
                (list)]
               [(and (list? (cadr xe)) ; '(foo a b c) attrs might be a
                     (andmap xexpr-attribute? (cadr xe)))
                (cadr xe)]
               [else
                (list)])]
        [else
         (list)]))

(: xexpr-children (-> XExpr
                      (Listof XExpr)))
(define (xexpr-children xe)
  (cond [(list? xe)
         (cond [(null? (cdr xe)) ; '(foo)
                (list)]
               [(and (list? (cadr xe)) ; '(foo a b c) attrs might be a
                     (andmap xexpr-attribute? (cadr xe)))
                (cddr xe)]
               [else
                (cdr xe)])]
        [else
         (list)]))

(: xexpr-attr->xdm (-> XExprAttribute
                       element-node
                       attribute-node))
(define (xexpr-attr->xdm attr parent)
  (attribute-node parent
                  (list)
                  (symbol->string (car attr))
                  (cadr attr)))

(: xexpr->xdm (->* (XExpr)
                   ((Option XDMNode))
                   XDMNode))
(define (xexpr->xdm xe [parent #f])
  (cond [(or (string? xe)
             (symbol? xe))
         (text-node parent (list) (list xe))]
        [(char? xe)
         (text-node parent (list) (list (format "~a" xe)))]
        [else
         (define new-node (element-node parent
                                        (list)
                                        (symbol->string (car xe))
                                        (list)))
         (define attrs (map (lambda ([a : XExprAttribute])
                              (xexpr-attr->xdm a new-node))
                            (xexpr-attrs xe)))
         (define kids (map (lambda ([x : XExpr])
                             (xexpr->xdm x new-node))
                           (xexpr-children xe)))
         (set-element-node-attributes! new-node attrs)
         (set-node-children! new-node kids)
         new-node]))
