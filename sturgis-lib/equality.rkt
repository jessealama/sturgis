#lang typed/racket/base

(provide xdm-equal?
         atomize)

(require racket/set
         racket/list
         racket/bool
         "types.rkt"
         "accessors.rkt")

(: symbol-sets-equal? (-> (Listof Symbol)
                          (Listof Symbol)
                          Boolean))
(define (symbol-sets-equal? symbols1 symbols2)
  (cond [(null? symbols1)
         (null? symbols2)]
        [(null? symbols2)
         #f]
        [else
         (define i (index-of symbols2
                             (car symbols1)
                             symbol=?))
         (cond [(eq? #f i)
                #f]
               [else
                (symbol-sets-equal? (cdr symbols1)
                                    (append (take symbols2 i)
                                            (drop symbols2 (add1 i))))])]))

(: attribute-sets-equal? (-> (Listof attribute-node)
                             (Listof attribute-node)
                             Boolean))
(define (attribute-sets-equal? attrs1 attrs2)
  (cond [(null? attrs1)
         (null? attrs2)]
        [(null? attrs2)
         #f]
        [else
         (define attr (car attrs1))
         (define attr-name (attribute-node-name attr))
         (define attr-value (attribute-node-value attr))
         (define i (index-of attrs2
                             attr
                             attributes-equal?))
         (cond [(eq? #f i)
                #f]
               [else
                (attribute-sets-equal? (cdr attrs1)
                                       (append (take attrs2 i)
                                               (drop attrs2 (add1 i))))])]))

(: elements-equal? (-> element-node
                       element-node
                       Boolean))
(define (elements-equal? elem1 elem2)
  (and (string=? (element-node-name elem1)
                 (element-node-name elem2))
       (attribute-sets-equal? (element-node-attributes elem1)
                              (element-node-attributes elem2))
       (xdm-equal? (node-children elem1)
                   (node-children elem2))))

(: attributes-equal? (-> attribute-node
                         attribute-node
                         Boolean))
(define (attributes-equal? attr1 attr2)
  (string=? (attribute-node-name attr1)
            (attribute-node-name attr2))
  (string=? (attribute-node-value attr1)
            (attribute-node-value attr2)))

(: processing-instructions-equal? (-> processing-instruction-node
                                      processing-instruction-node
                                      Boolean))
(define (processing-instructions-equal? proc1 proc2)
  (and (string=? (processing-instruction-node-target proc1)
                 (processing-instruction-node-target proc2))
       (string=? (processing-instruction-node-instruction proc1)
                 (processing-instruction-node-instruction proc2))))

(: text-nodes-equal? (-> text-node
                         text-node
                         Boolean))
(define (text-nodes-equal? text1 text2)
  (string=? (string-value text1)
            (string-value text2)))

(: comments-equal? (-> comment-node
                       comment-node
                       Boolean))
(define (comments-equal? comment1 comment2)
  (string=? (string-value comment1)
            (string-value comment2)))

(: documents-equal? (-> document-node
                        document-node
                        Boolean))
(define (documents-equal? doc1 doc2)
  (define uri1 (document-node-uri doc1))
  (define uri2 (document-node-uri doc2))
  (and (or (and (eq? #f uri1)
                (eq? #f uri2))
           (and (string? uri1)
                (string? uri2)
                (string=? uri1 uri2)))
       (xdm-equal? (node-children doc1)
                   (node-children doc2))))

(: namespace-nodes-equal? (-> namespace-node
                              namespace-node
                              Boolean))
(define (namespace-nodes-equal? ns1 ns2)
  (and (equal? (namespace-node-prefix ns1)
               (namespace-node-prefix ns2))
       (equal? (namespace-node-uri ns1)
               (namespace-node-uri ns2))))

(: xdm-maps-equal? (-> xdm-map
                       xdm-map
                       Boolean))
(define (xdm-maps-equal? map1 map2)
  (define h1 (xdm-map-data map1))
  (define h2 (xdm-map-data map2))
  (and (symbol-sets-equal? (hash-keys h1)
                           (hash-keys h2))
       (andmap (lambda ([k : Symbol])
                 (xdm-equal? (hash-ref h1 k)
                             (hash-ref h2 k)))
               (hash-keys h1))))

(: equal-xdm-sequences? (-> (Listof XDMValue)
                            (Listof XDMValue)
                            Boolean))
(define (equal-xdm-sequences? seq1 seq2)
  (cond [(null? seq1)
         (null? seq2)]
        [(null? seq2)
         #f]
        [(xdm-equal? (car seq1) (car seq2))
         (equal-xdm-sequences? (cdr seq1) (cdr seq2))]
        [else
         #f]))

(: xdm-arrays-equal? (-> xdm-array
                         xdm-array
                         Boolean))
(define (xdm-arrays-equal? arr1 arr2)
  (equal-xdm-sequences? (xdm-array-entries arr1)
                        (xdm-array-entries arr2)))

(: xdm-items-equal? (-> XDMItem
                        XDMItem
                        Boolean))
(define (xdm-items-equal? thing1 thing2)
  (cond [(string? thing1)
         (cond [(string? thing2)
                (string=? thing1 thing2)]
               [(attribute-node? thing2)
                (string=? thing1 (attribute-node-value thing2))]
               [else
                (error "Don't know to compare string \"~a\" with ~a"
                       thing1
                       thing2)])]
        [(number? thing1)
         (and (number? thing2)
              (= thing1 thing2))]
        [(boolean? thing1)
         (and (boolean? thing2)
              (equal? thing1 thing2))]
        [(xdm-map? thing1)
         (and (xdm-map? thing2)
              (xdm-maps-equal? thing1 thing2))]
        [(xdm-array? thing1)
         (and (xdm-array? thing2)
              (xdm-arrays-equal? thing1 thing2))]
        [(attribute-node? thing1)
         (cond [(attribute-node? thing2)
                (attributes-equal? thing1 thing2)]
               [(string? thing2)
                (string=? (attribute-node-value thing1)
                          thing2)]
               [else
                (error "Don't know how to compare ~a attribute (value = ~a) with ~a"
                       (attribute-node-name thing1)
                       (attribute-node-value thing1)
                       thing2)])]
        [(element-node? thing1)
         (and (element-node? thing2)
              (elements-equal? thing1 thing2))]
        [(document-node? thing1)
         (and (document-node? thing2)
              (documents-equal? thing1 thing2))]
        [(processing-instruction-node? thing1)
         (and (processing-instruction-node? thing2)
              (processing-instructions-equal? thing1 thing2))]
        [(namespace-node? thing1)
         (and (namespace-node? thing2)
              (namespace-nodes-equal? thing1 thing2))]
        [(text-node? thing1)
         (and (text-node? thing2)
              (text-nodes-equal? thing1 thing2))]
        [(comment-node? thing1)
         (and (comment-node? thing2)
              (comments-equal? thing1 thing2))]))

(: atomize (-> XDMValue
               (Listof AtomicXDMItem)))
(define (atomize value)
  (cond [(atomic-xdm-item? value)
         (list value)]
        [(xdm-map? value)
         (error "Cannot atomize an XDM map")]
        [(xdm-array? value)
         (apply append
                (map atomize (xdm-array-entries value)))]
        [(xdm-node? value)
         (list (string-value value))]
        [(xdm-sequence? value)
         (apply append
                (map atomize value))]
        [else
         (error "Don't know hot to atomize XDM value ~a" value)]))

(: xdm-equal? (-> XDMValue
                  XDMValue
                  Boolean))
(define (xdm-equal? thing1 thing2)
  (cond [(xdm-item? thing1)
         (cond [(xdm-item? thing2)
                (xdm-items-equal? thing1 thing2)]
               [(null? thing2)
                #f]
               [else
                #f])]
        [(null? thing1)
         #f]
        [(xdm-item? thing2)
         (equal-xdm-sequences? thing1 (list thing2))]
        [(null? thing2)
         #f]
        [else
         (equal-xdm-sequences? thing1 thing2)]))
