#lang racket

(require "./particle.rkt")

; Starts a simulation with the given width and height,
; creating a Scratch-style one-dimensional list to store
; the particles. The default and border particle should be a particle,
; not a particle ID. The list is initialized with the default particle
; and references to outside the map returns the border particle.
(define (start-sim width height default-part border-part)
  (define (create-map-iter cells-left map)
    (if (<= cells-left 0) map
        (create-map-iter (- cells-left 1) (mcons default-part map))))

  (define curr-map (create-map-iter (* width height) '()))

  ; Returns a callback lambda that can be called to either
  ; set a particle or simulate a particle:
  ; (step 'set 1 2 (assoc 'water particle-types))
  ; (step 'sim 1 2)
  (define (step instruct x y [to-part #f])
    ; Gets the Nth pair in the map; zero-indexed.
    (define (get-nth-pair n)
      (define (iter i ls)
        (if (<= i 0) ls
            (iter (- i 1) (mcdr ls))))
      (iter n curr-map))
    (cond ((or (< x 0) (< y 0) (>= x width) (>= y height))
           (error "Hmm! Quite suspish! " x " " y))
          ((equal? instruct 'set)
           (set-mcar! (get-nth-pair (+ x (* y width))) to-part))
          ((equal? instruct 'sim)
           (for-each
            (lambda (replacement)
              (define px (+ x (caar replacement)))
              (define py (+ y (cdar replacement)))
              (when (and (>= px 0) (>= py 0) (< px width) (< py height))
                (set-mcar! (get-nth-pair (+ px (* py width)))
                           (assoc (cadr replacement) particle-types))))
            ((particle-fn (mcar (get-nth-pair (+ x (* y width)))))
             (lambda (offset-x offset-y)
               (define px (+ x offset-x))
               (define py (+ y offset-y))
               (if (or (< px 0) (< py 0)
                       (>= px width) (>= py height))
                   (particle-id border-part)
                   (particle-id
                    (mcar (get-nth-pair (+ px (* py width))))))))))
          (else
           (error "What kind of instruction is " instruct "???")))
    sim)
  (define sim (list step width height curr-map))
  sim)

(provide start-sim)
