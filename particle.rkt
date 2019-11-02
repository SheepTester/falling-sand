#lang racket

; This is needed to access make-color.
(require racket/draw)

(define particle list)
(define particle-name car)
(define particle-colour cadr)

(define particle-types
  (list (particle "Empty" (make-color 159 222 242))
        (particle "Metal" (make-color 78 83 88))
        (particle "Sand" (make-color 212 203 178))
        (particle "Water" (make-color 57 186 230))))

(provide particle)
(provide particle-name)
(provide particle-colour)
(provide particle-types)