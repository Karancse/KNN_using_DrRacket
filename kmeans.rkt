#lang racket

(require racket/trace)

(define (get-random-means dataset)
  )

(define (calculate-clusters dataset means)
  )
 
(define (update-means dataset clusters)
  (calculate-clusters dataset)
  )

(define (kmeans-iter dataset means)
  (define clusters (calculate-clusters dataset means))
  (define updated-means (update-means dataset clusters))
  (if (= updates-means means)
      clusters
      (kmeans-iter dataset updated-means)))

(define (kmeans-get-user-inputs)
  (display "Enter the value of 'k'\n    ")
  (define k (read))
  (display "Enter the number of data points\n    ")
  (define number-of-datapoints (read))
  (display "Enter the data points\n    ")
  (define dataset (get-dataset number-of-datapoints))
  (display dataset)
  (list k number-of-datapoints dataset))

(define (kmeans)
  (define user-inputs (kmeans-get-user-inputs))
  (define k (car user-inputs))
  (define number-of-datapoints (car (cdr user-inputs)))
  (define dataset (car (cdr (cdr user-inputs))))
  (define means (get-random-means dataset))
  (define clusters (kmeans-iter dataset means)))


