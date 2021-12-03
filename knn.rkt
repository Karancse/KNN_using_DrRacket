#lang racket
(require racket/trace)

(define (read-datapoint number-of-dimensions)
  (if (= number-of-dimensions 1)
      (read)
      (let ([x (cons (read) (read-datapoint (- number-of-dimensions 1)))])
        (print x)
        x
      )
      ))

(define (get-dataset number-of-datapoints number-of-dimensions)
    (if (= number-of-datapoints 0)
        (list)
        (cons (read-datapoint number-of-dimensions) (get-dataset (- number-of-datapoints 1) number-of-dimensions))))

(define (get-classes number-of-datapoints)
    (if (= number-of-datapoints 1)
        (read)
        (
         let ([x (cons (read) (get-classes (- number-of-datapoints 1)))])
        (print x)
          x
          )))

(define (first-datapoint dataset)
  (if (pair? dataset)
      (car dataset)
      dataset)
  )

(define (remaining-datapoints dataset)
  (if (pair? dataset)
      (cdr dataset)
      (list)))

(define (first-dimension datapoint)
  (if (pair? datapoint)
      (car datapoint)
      datapoint
      ))

(define (remaining-dimensions datapoint)
  (if (pair? datapoint)
      (cdr datapoint)
      (list)
      ))

(define (first values)
  (if (list? values)
      (car values)
      values))

(define (remaining values)
  (if (pair? values)
      (cdr values)
      (list)))

(define (iter-dataset iter)
  (car iter))

(define (iter-classes iter)
  (car (cdr iter)))

(define (iter-distances iter)
  (cdr (cdr iter)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (square-root-iter guess x)
  (if (good-enough? guess x)
      guess
      (square-root-iter (improve guess x)
                 x)))

(define (square-root x)
  (square-root-iter 1.0 x))

(define (square x)
  (* x x))

(define (squared-distance datapoint1 datapoint2)
  (if (eq? '() datapoint1)
      0
      (+ (square (- (first-dimension datapoint1) (first-dimension datapoint2))) (squared-distance (remaining-dimensions datapoint1) (remaining-dimensions datapoint2)))))

(define (euclidean-distance datapoint1 datapoint2)
  (square-root (squared-distance datapoint1 datapoint2)))

(define (calculate-distances dataset value-to-be-classified)
  (if (eq? '() dataset)
      '()
      (cons (euclidean-distance (first-datapoint dataset) value-to-be-classified ) (calculate-distances (remaining-datapoints dataset) value-to-be-classified))))

(define (insert datapoint class distance dataset classes distances)
  display(datapoint)
  display(" ")
  display(class)
  display(distance)
  display(" ")
  display(dataset)
  display(" ")
  display(classes)
  display(" ")
  display(distances)
  display("\n")
  (if (eq? (list) dataset)
      (datapoint class distance)
      (if (< distance (first distances))
          ((cons datapoint dataset) (cons class classes) (cons distance distances))
          (let ([previous-iter (insert datapoint class distance (remaining dataset) (remaining classes) (remaining distances))])
              ((cons (first dataset) (iter-dataset previous-iter)) (cons (first classes) (iter-classes previous-iter)) (cons (first distances) (iter-distances previous-iter))) 
              )
          )
      )
  )

(define (sort-by-distance dataset classes distances)
  (if (eq? (list) dataset)
      -1
      (let ([previous-iter (sort-by-distance (remaining dataset) (remaining classes) (remaining distances))])
        (if (number? previous-iter)
            (list dataset classes distances)
            (insert (first dataset) (first classes) (first distances) (iter-dataset previous-iter) (iter-classes previous-iter) (iter-distances previous-iter))
            )
        ))
  )

(define (find-class unique-values class-count)
  (if (eq? (list) unique-values)
      (-1 -1)
      (let ([remaining-maximum (find-class (remaining unique-values) (remaining class-count))])
        (if (> (first class-count) (remaining remaining-maximum))
            ((first unique-values) (first class-count))
            remaining-maximum))))

(define (count-frequency sorted-classes class)
  (if (eq? (list) sorted-classes)
      0
      (+ (if (= (first sorted-classes) class)
             1
             0)
             (count-frequency (remaining sorted-classes) class))))

(define (count-list-frequency sorted-classes unique-classes)
  (if (or (eq? (list) unique-classes))
      (list)
      (cons (count-frequency sorted-classes (first unique-classes)) (count-list-frequency (remaining unique-classes)))))

(define (member? element elements)
  (if (eq? (list) elements)
      #f
      (if (= element (first elements))
          #t
          (member element (remaining elements))
          )
  ))

(define (unique-values sorted-classes)
  (if (eq? (list) sorted-classes)
      (list)
      (let ([previous-iter (unique-values (remaining sorted-classes))])
        (if (member (first sorted-classes) previous-iter)
            previous-iter
            (cons (first sorted-classes) previous-iter)
            )
        )
      )
  )

(define (classify value-to-be-classified sorted-dataset sorted-classes)
  (define unique-classes (unique-values sorted-classes))
  (define class-count (count-list-frequency sorted-classes unique-classes))
  (define class (find-class unique-values class-count))
  class
  )

(define (find-nearest-point dataset value-to-be-classified)
  (if (empty? dataset)
      (list +inf.0 'null 'null)
      (let ([distance (euclidean-distance (first-datapoint dataset) value-to-be-classified)]
            [opt-distance (classify (remaining-datapoints dataset) value-to-be-classified)])
         (if (< distance (car opt-distance))
             (list distance (car (car dataset)) (cdr (car dataset)))
             opt-distance))))

(define (knn-get-user-inputs)
  (display "Enter the value of 'k'\n    ")
  (define k (read))
  (display "Enter the number of dimensions\n    ")
  (define number-of-dimensions (read))
  (display "Enter the number of data points\n    ")
  (define number-of-datapoints (read))
  (display "Enter the data points\n    ")
  (define dataset (get-dataset number-of-datapoints number-of-dimensions))
  (display "Enter the classes corresponding to the data points\n    ")
  (define classes (get-classes number-of-datapoints))
  (display dataset)
  (display "\nEnter the value to be classified\n    ")
  (define value-to-be-classified (read-datapoint number-of-dimensions))
  (list k number-of-dimensions number-of-datapoints dataset classes value-to-be-classified))

(define (knn)
  (define user-inputs (knn-get-user-inputs))
  (define k (car user-inputs))
  (display "\nk\n    ")
  (display k)
  (define number-of-dimensions (car (cdr user-inputs)))
  (display "\nnumber-of-dimensions\n    ")
  (display number-of-dimensions)
  (define number-of-datapoints (car (cdr (cdr user-inputs))))
  (display "\nnumber-of-datapoints\n    ")
  (display number-of-datapoints)
  (define dataset (car (cdr (cdr (cdr user-inputs)))))
  (display "\ndataset\n    ")
  (display dataset)
  (define classes (car (cdr (cdr (cdr (cdr user-inputs))))))
  (display "\nclasses\n    ")
  (display classes)
  (define value-to-be-classified (car (cdr (cdr (cdr (cdr (cdr user-inputs)))))))
  (display "\nvalue-to-be-classified\n    ")
  (display value-to-be-classified)
  (define distances (calculate-distances dataset value-to-be-classified))
  (display "\nDistances\n    ")
  (display distances)
  (define sorted-values (sort-by-distance dataset classes distances))
  (define sorted-dataset (car sorted-values))
  (display "\nsorted-dataset\n    ")
  (display sorted-dataset)
  (define sorted-classes (car (cdr sorted-values)))
  (display "\nsorted-classes\n    ")
  (display sorted-classes)
  (define sorted-distances (cdr (cdr sorted-values)))
  (display "\nsorted-distances\n    ")
  (display sorted-distances)
  """(classify value-to-be-classified sorted-dataset sorted-classes)
""")

(define (algo-choice algo)
  (case algo
   [(1) 'linearregression]
   [(2) knn]
   [(3) 'kmeans]
   [else 'invalid]
  )
)

(display "Welcome to Machine Learning Program \n Enter 1 for linear regression \n       2 for KNN \n       3 for k-means \n       ")
(define algo (read))

((algo-choice algo))
