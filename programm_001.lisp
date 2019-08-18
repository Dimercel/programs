;; Данная программа представляет собой реализацию простейшего целочисленного калькулятора.


;; Начнем с описания допустимых операторов в нашем калькуляторе. К ним мы относим базовые
;; арифметические операции, плюс квадратные скобки, которые меняют приоритет.
(defun operatorp (token) (member token '(+ - * / [ ])))

;; Изначально, выражение представлено в инфиксной записи, поэтому важны приоритеты операторов.
;; Их мы определим в следующей функции.
(defun op-lvl (token)
  (cond
    ((eq token '[) 0)
    ((eq token ']) 1)
    ((eq token '+) 2)
    ((eq token '-) 2)
    ((eq token '*) 3)
    ((eq token '/) 3)))

(defun take-while (pred seq)
  (if (null seq)
      '()
      (when (funcall pred (first seq))
        (cons (first seq)
              (take-while pred (rest seq))))))

(defun to-postfix (expr &optional (stack '()) (result '()))
  "Преобразует выражение из инфиксной в постфиксную запись"
  (let ((token (first expr)))
    (if token
      (cond
        ((numberp token)
         (to-postfix (rest expr) stack (append result (list token))))
        ((eq token '[)
         (to-postfix (rest expr) (cons token stack) result))
        ((eq token '])
         (let ((pop-op (take-while (lambda (x)
                                     (not (eq x '[)))
                                   stack)))
           (to-postfix
            (rest expr)
            (subseq stack (1+ (length pop-op)))
            (append result pop-op))))
        ((operatorp token)
         (let ((pop-op (take-while (lambda (x)
                                      (>= (op-lvl x) (op-lvl token)))
                                    stack)))
           (to-postfix
            (rest expr)
            (cons token (subseq stack (length pop-op)))
            (append result pop-op)))))
      (append result stack))))

(defun calculate (expr)
  "Вычисляет результат выражения,
  находящиеся в постфиксной записи"
  (let ((stack nil))
    (dolist (token expr)
      (if (numberp token)
          (push token stack)
          (let ((op-2 (pop stack))
                (op-1 (pop stack)))
            (push (funcall token op-1 op-2)
                  stack))))
    (first stack)))
