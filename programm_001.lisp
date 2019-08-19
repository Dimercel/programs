;; Данная программа представляет собой реализацию простейшего целочисленного калькулятора.


(defun take-while (pred seq)
  "Вспомогательная функция которая нам потребуется при
   конвертации в постфиксную запись"
  (if (null seq)
      '()
      (when (funcall pred (first seq))
        (cons (first seq)
              (take-while pred (rest seq))))))

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

;; Прежде чем вычислять выражение нам необходимо привести его из инфиксной формы в
;; постфиксную. Для этого воспользуемся алгоритмом Дейкстры.
;; https://ru.wikipedia.org/wiki/Обратная_польская_запись
(defun to-postfix (expr &optional (stack '()) (result '()))
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

;; Уже на данном этапе можно насладиться плодами и перевести выражение из инфиксной
;; формы в постфиксную:
;; (to-postfix '([ 1 + 2 ] * 4 + 3)) => '(1 2 + 4 * 3 +)
;; (to-postfix '(3 + [ 10 - 15 ])) => '(3 10 15 - +)

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
