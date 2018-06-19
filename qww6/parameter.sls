(library (qww6 parameter)
  (export make-cm-parameter cm-parameterize)
  (import (chezscheme) (qww6 contmarks))

  (define parameterization-key (make-continuation-mark-key))

  (define (make-cm-parameter v)
    (letrec ([k
              (case-lambda
                [()
                 (define l (continuation-mark-set->list (current-continuation-marks)
                                                        parameterization-key))
                 (let loop ([l l])
                   (if (null? l)
                       v
                       (if (eq-hashtable-contains? (car l) k)
                           (eq-hashtable-ref (car l) k #f)
                           (loop (cdr l)))))]
                [(new-v)
                 (let ([l (continuation-mark-set->list (current-continuation-marks)
                                                       parameterization-key)])
                   (let loop ([l l])
                     (if (null? l)
                         (set! v new-v)
                         (if (eq-hashtable-contains? (car l) k)
                             (eq-hashtable-set! (car l) k new-v)
                             (loop (cdr l))))))])])
      k))

  (define-syntax (cm-parameterize stx)
    (syntax-case stx ()
      [(_ ([key value] ...) body body* ...)
       (with-syntax ([(k ...) (generate-temporaries #'(key ...))]
                     [(v ...) (generate-temporaries #'(value ...))])
         #'(let ([k key] ...)
             (let ([v value] ...)
               (let ([thunk (lambda (e)
                              (eq-hashtable-set! e k v) ...
                              (let ()
                                body
                                body* ...))])
                 (call-with-immediate-continuation-mark
                  parameterization-key
                  (lambda (e)
                    (if e
                        (thunk e)
                        (let ([e (make-eq-hashtable)])
                          (with-continuation-mark
                           parameterization-key e
                           (thunk e))))))))))]))
  )
