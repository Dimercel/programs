;; Алгоритм построения выпуклой оболочки на плоскости

(ql:quickload :trivia)

(defpackage :quickhull
  (:use :cl
        :trivia
        :vecto))

(in-package :quickhull)


(defun square-dist (p0 p1)
  (multiple-value-match (values p0 p1)
    (((vector x0 y0) (vector x1 y1))
     (+ (expt (- x1 x0) 2) (expt (- y1 y0) 2)))))

(defun distance (p0 p1)
  (sqrt (square-dist p0 p1)))

(defun dist-to-line (p0 p1 outside)
  (multiple-value-match (values p0 p1 outside)
    (((vector x0 y0) (vector x1 y1) (vector mx my))
     (let ((first-step (abs (+ (* (- y1 y0) mx)
                               (- (* (- x1 x0) my))
                               (* x1 y0)
                               (* (- y1) x0))))
           (dist (square-dist p0 p1)))
       (when (/= dist 0)
         (/ first-step dist))))))

(defun farthest-point (p0 p1 points)
  (when points
    (match points
      ((cons fp _)
       (when (not (equalp p0 p1))
         (first
          (reduce (lambda (acc x) (if (> (dist-to-line p0 p1 x) (second acc))
                                      (list x (dist-to-line p0 p1 x))
                                      acc))
                  points
                  :initial-value (list fp (dist-to-line p0 p1 fp)))))))))

(defun limit-left-point (points)
  (reduce (lambda (acc x) (if (< (aref x 0) (aref acc 0)) x acc))
           points
           :initial-value (first points)))

(defun limit-right-point (points)
  (reduce (lambda (acc x) (if (> (aref x 0) (aref acc 0)) x acc))
          points
          :initial-value (first points)))

(defun middle-point (p0 p1)
  (multiple-value-match (values p0 p1)
    (((vector x0 y0) (vector x1 y1))
     (vector (/ (+ x0 x1) 2)
             (/ (+ y0 y1) 2)))))

(defun separate-points (p0 p1 points)
  (multiple-value-match (values p0 p1)
    (((vector x0 y0) (vector x1 y1))
     (let* ((a (- y1 y0))
            (b (- x0 x1))
            (c (- (* x0 y1) (* y0 x1)))
            (half-plane1 (lambda (x) (> (+ (* a (aref x 0)) (* b (aref x 1))) c)))
            (half-plane2 (lambda (x) (< (+ (* a (aref x 0)) (* b (aref x 1))) c))))
       (vector (remove-if-not half-plane1 points)
               (remove-if-not half-plane2 points))))))

(defun quick-hull (p0 p1 center points)
  (if (not points)
      (list (vector p0 p1))
      (let* ((dp (farthest-point p0 p1 points))
             (first-segment (separate-points p0 dp (cons center points)))
             (outside1 (if (member center (aref first-segment 0))
                           (aref first-segment 1)
                           (aref first-segment 0)))
             (second-segment (separate-points p1 dp (cons center points)))
             (outside2 (if (member center (aref second-segment 0))
                           (aref second-segment 1)
                           (aref second-segment 0))))
        (concatenate 'list
                     (quick-hull p0 dp center outside1)
                     (quick-hull p1 dp center outside2)))))

(defun main (points)
  (let* ((limit-left (limit-left-point points))
         (limit-right (limit-right-point points))
         (separated (separate-points limit-left limit-right points))
         (half-plane1 (aref separated 0))
         (half-plane2 (aref separated 1))
         (center (middle-point limit-left limit-right)))
    (concatenate 'list
                 (quick-hull limit-left limit-right center half-plane1)
                 (quick-hull limit-left limit-right center half-plane2))))

;; Пример использования: (main (list #(-10 0) #(10 0) #(-3 4) #(-1 7) #(5 3) #(9 1) #(-2 -7)))
