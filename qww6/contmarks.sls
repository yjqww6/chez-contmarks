#!chezscheme
(library (qww6 contmarks)
  (export continuation-marks
          current-continuation-marks
          with-continuation-mark
          make-continuation-mark-key
          continuation-mark-set->list
          continuation-mark-set-first
          continuation-mark-set->list*
          call-with-immediate-continuation-mark
          continuation-mark-set?
          continuation-mark-key?
          )
  (import (chezscheme))

  (define *marks* (make-weak-eq-hashtable))

  (meta-cond
   [threaded?
    (define *mutex* (make-mutex))
    (define-syntax locked
      (syntax-rules ()
        [(_ body body* ...)
         (with-mutex *mutex* body body* ...)]))]
   [else (define-syntax locked
           (syntax-rules ()
             [(_ body body* ...)
              (let () body body* ...)]))])

  (define-record-type continuation-mark-set (fields value))

  (define (continuation-marks c)
    (make-continuation-mark-set
     (locked
      (let loop ([c c])
        (cond
         [(eq? c #%$null-continuation) '()]
         [(eq-hashtable-ref *marks* c #f) =>
          (lambda (v)
            (cons (hashtable-copy v #f) (loop (#%$continuation-link c))))]
         [else (loop (#%$continuation-link c))])))))

  (define (current-continuation-marks)
    (call/1cc
     (lambda (cc)
       (continuation-marks cc))))

  (define-syntax with-continuation-mark
    (syntax-rules ()
      [(_ key-expr val-expr result-expr)
       (let ([k key-expr] [v val-expr])
         (call/1cc
          (lambda (cc)
            (define cell (locked (eq-hashtable-cell *marks* cc #f)))
            (cond [(cdr cell) =>
                   (lambda (e)
                     (eq-hashtable-set! e k v))]
                  [else
                   (let ([e (make-weak-eq-hashtable)])
                     (eq-hashtable-set! e k v)
                     (set-cdr! cell e))])
            result-expr)))]))

  (define-record-type continuation-mark-key (fields name)
    (protocol
     (lambda (new)
       (case-lambda
         [() (new (gensym))]
         [(name) (new name)]))))

  (define (continuation-mark-set->list mark-set key-v)
    (let loop ([l (continuation-mark-set-value  mark-set)])
      (cond
       [(null? l) '()]
       [(eq-hashtable-contains? (car l) key-v)
        (cons (eq-hashtable-ref (car l) key-v #f) (loop (cdr l)))]
       [else (loop (cdr l))])))

  (define continuation-mark-set-first
    (case-lambda
      [(mark-set key-v)
       (continuation-mark-set-first mark-set key-v #f)]
      [(mark-set key-v none-v)
       (if mark-set
           (let loop ([m (continuation-mark-set-value mark-set)])
             (cond
              [(null? m) none-v]
              [(eq-hashtable-contains? (car m) key-v)
               (eq-hashtable-ref (car m) key-v #f)]
              [else (loop (cdr m))]))
           (call/1cc
            (lambda (cc)
              (locked
               (let loop ([cc cc])
                 (cond
                  [(eq? cc #%$null-continuation) none-v]
                  [(eq-hashtable-ref *marks* cc #f) =>
                   (lambda (v)
                     (cond
                      [(eq-hashtable-contains? v key-v)
                       (eq-hashtable-ref v key-v #f)]
                      [else (loop (#%$continuation-link cc))]))]
                  [else (loop (#%$continuation-link cc))]))))))]))

  (define continuation-mark-set->list*
    (case-lambda
      [(mark-set key-list)
       (continuation-mark-set->list* mark-set key-list #f)]
      [(mark-set key-list none-v)
       (define len (length key-list))
       (define (get-key k a vec pos)
         (cond
          [(eq-hashtable-contains? a k)
           (vector-set! vec pos (eq-hashtable-ref a k #f))]
          [else (vector-set! vec pos none-v)]))
       (define (get-key-list a)
         (define vec (make-vector len))
         (let loop ([l key-list] [i 0])
           (cond
            [(null? l) vec]
            [else (get-key (car l) a vec i)
                  (loop (cdr l) (+ i 1))])))
       (map get-key-list (continuation-mark-set-value mark-set))]))

  (define call-with-immediate-continuation-mark
    (case-lambda
      [(key-v proc)
       (call-with-immediate-continuation-mark key-v proc #f)]
      [(key-v proc default-v)
       (call/1cc
        (lambda (cc)
          (cond
           [(locked (eq-hashtable-ref *marks* cc #f)) =>
            (lambda (v)
              (cond
               [(eq-hashtable-contains? v key-v)
                (proc (eq-hashtable-ref v key-v #f))]
               [else (proc default-v)]))]
           [else (proc default-v)])))]))

  )
