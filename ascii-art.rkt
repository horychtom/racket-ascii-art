#lang racket
(require 2htdp/image)
(require rackunit)
(require racket/trace)

(provide img->mat
ascii-art)
 




(define imag (triangle 5 "solid" "violet"))

;UTILS------------------------------------------

(define (RGB->grayscale color)
    (+ (* 0.3 (color-red color))
       (* 0.59 (color-green color))
       (* 0.11 (color-blue color))
    )
)

;computes intensity based on predefined formula
(define (get-intensity inte charlen)
    (if (not (number? inte))
         -1
        (exact-floor (floor (/ (* charlen (- 255 (floor inte))) 256)))
    )
)


(define (split-list n lst)

  (define (iter l k segment)
    (cond
      ([null? l] (list segment))
      ([= k 0] (cons segment (iter l n '())))
      (else (iter (cdr l) (- k 1) (append segment (list (car l)))))
      )
    )
 
  (iter lst n '())
)

(define (cut-list lst idx [acc '()])
    (cond
        ([> idx (length lst)] '())
        ([= idx 0] acc)
        (else (cut-list (cdr lst) (- idx 1) (append acc (list(car lst)))))
     )
)
;------------------------------------------------

; (1 2 3 4) -> ((1 2) 3 4)
(define (get-row lst-origin count [acc '()])
        (if (= count 0)
            (cons acc lst-origin)
            (get-row (cdr lst-origin) (- count 1) (append acc (list(car lst-origin))))
        )
)

(define (img->mat img)
    (define wid (image-width img))

    (define (rowise remainder)
        (cond ([null? remainder] '())
            (else
                (define semiresult (get-row remainder wid '()))
                (cons (car semiresult) (rowise (cdr semiresult)))
            )
        )
    )
 
    ;1d list of intensities goes matrix
    (rowise (map RGB->grayscale (image->color-list img)))
)

(define (get-final-string lst cols acc idx)

    (cond
        ([and (null? lst) (not(= idx 0))] "")
        ([null? lst] (string-append acc "\n"))
        ([= idx 0] (get-final-string lst cols (string-append acc "\n") cols))
        (else (get-final-string (cdr lst) cols (string-append acc (string (car lst))) (- idx 1)))
    )

)

;((1 2) (3 4)) -> (3 7)
(define (sum-in-rows mat)
    (map (lambda (y) (map (lambda (x) (apply + x)) y)) mat)
)


(define (split-rows width mat)
    (map (lambda (y) (split-list width y))mat )
)

(define (pre-cut-matrix columns rows mat)
    (define mat-lst (cut-list mat rows '()))
    
    (if (null? mat-lst) '()
    (map (lambda (x) (cut-list x columns '())) mat-lst))
)

(define (final-sum mat)
    (if (null? (car mat)) '()
    (map (lambda (y) (apply map + y)) mat))
)

(define (list-to-string lst size chars)
    (if (null? lst) '()
    (map (lambda (x) (list-ref chars x))
        (map (lambda (x) (get-intensity x (length chars)))
            (map (lambda (x) (/ x size)) lst))))
)


;spatial matrix manipulation 
;1) cuts of columns and rows that cannot be used
;2) split each row into blocks
;3) adds these blocks ((1 2) (3 5)) -> (3 8)
;4) split matrix in second direction (rows together) into blocks
;5) adds these blocks together and flattens into 1d list
(define (mat-reduction width height mat)
    (define columns (* width (floor (/ (length (car mat)) width))))
    (define rows (* height (floor (/ (length mat) height))))

    (flatten(final-sum
            (split-list height
                (sum-in-rows
                    (split-rows width
                        (pre-cut-matrix columns rows mat))))))
)
    
    

(define (ascii-art width height chars)
    (define size (* height width))

    (lambda (img)
        (if (= (image-width img) 0) ""
        (get-final-string
            (list-to-string
                (mat-reduction width height (img->mat img))
                    size (string->list chars))
                        (quotient (image-width img) width) "" (quotient (image-width img) width))))
    
)



(display ((ascii-art 1 2 " .,:;ox%#@")  (bitmap "my.png")))


;TESTS--------------------------------------------------------------


 
(check-equal? (get-row '(1 2 3 4) 2 '()) '((1 2) 3 4) "You made an error!")
(check-equal? (img->mat imag) '((255.0 255.0 174.28 180.07 255.0)
                              (255.0 174.69 174.28 174.1 255.0)
                              (171.46 174.28 174.28 174.28 172.69)
                              (174.28 174.28 174.28 174.28 173.69)) "Matrix function crashed" )
(check-equal? (get-intensity 90 10) 6 "Wrong intensity")

;-------------------------------------------------------------------
