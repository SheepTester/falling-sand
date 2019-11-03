#lang racket

; This is needed to access make-color.
(require racket/draw)

(define particle list)
(define particle-id car)
(define particle-name cadr)
(define particle-colour caddr)
(define particle-fn cadddr)

(define particle-types
  (list (particle 'empty "Empty" (make-color 159 222 242)
                  (lambda (get) '()))
        (particle 'metal "Metal" (make-color 78 83 88)
                  (lambda (get) '()))
        (particle 'sand "Sand" (make-color 212 203 178)
                  (lambda (get)
                    (cond ((equal? (get 0 1) 'empty)
                           '(((0 . 0) empty) ((0 . 1) sand)))
                          ((equal? (get 0 1) 'water)
                           '(((0 . 0) water) ((0 . 1) sand)))
                          (else '()))))
        (particle 'water "Water" (make-color 57 186 230)
                  (lambda (get)
                    (let ((dir
                           (list-ref '((-1 . 0) (0 . 1) (1 . 0))
                                     (exact-floor (* (random) 3)))))
                      (if (equal? (get (car dir) (cdr dir)) 'empty)
                          (list '((0 . 0) empty) (list dir 'water))
                          '()))))))

(provide particle)
(provide particle-id)
(provide particle-name)
(provide particle-colour)
(provide particle-fn)

(provide particle-types)
