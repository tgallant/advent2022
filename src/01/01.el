;; https://adventofcode.com/2022/day/1

(require 'aoc)

(defun item-sort (acc cur)
  (let ((buf (car acc))
        (res (cdr acc)))
    (if (equal cur "")
        (cons '() (append res (list buf)))
      (cons (append buf (list cur)) res))))

(defun sort-inventory (lst)
  (cl-reduce 'item-sort lst :initial-value '()))

(defun sum (lst)
  (cl-reduce '+ lst))

(defun sum-calories (lst)
  (mapcar 'sum lst))

(defun max-calories (lst)
  (let* ((sorted (sort-inventory lst))
         (summed (sum-calories sorted)))
    (apply 'max (flatten-list summed))))

(defun descending-calories (lst)
  (let* ((sorted (sort-inventory lst))
         (summed (sum-calories sorted)))
    (sort (flatten-list summed) (lambda (a b) (> a b)))))

(defun top-three-calories (lst)
  (let ((vals (descending-calories lst)))
    (apply '+ (cl-subseq vals 0 3))))

(defun str-to-num (val)
  (if (equal val "") ""
    (string-to-number val)))

(defsolution-with-null part1
  (mapcar 'str-to-num)
  (max-calories))

(defsolution-with-null part2
  (mapcar 'str-to-num)
  (top-three-calories))

(defsolve "01"
  ((part1 "01.test.txt") 24000)
  ((part1 "01.input.txt") 67016)
  ((part2 "01.test.txt") 45000)
  ((part2 "01.input.txt") 200116))
