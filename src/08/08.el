;; https://adventofcode.com/2022/day/7

(require 'aoc)
(require 'subr-x)

(defun getrow (grid row-idx from to)
  (let ((row (nth row-idx grid)))
    (cl-subseq row from to)))

(defun getcol (grid col-idx from to)
  (defun nth-col (row)
    (nth col-idx row))
  (let ((col (mapcar 'nth-col grid)))
    (cl-subseq col from to)))

(defun getval (grid row col)
  (nth col (nth row grid)))

(defun make-pairs (rows cols)
  (defun row-pairs (acc cur)
    (append acc (mapcar (lambda (col) (list cur col)) cols)))
  (cl-reduce 'row-pairs rows :initial-value '()))

(defun edge? (up down left right)
  (member nil (list up down left right)))

(defun visible? (val up down left right)
  (defun is-greatest (dir)
    (not (member nil (mapcar (lambda (x) (> val x)) dir))))
  (member t (mapcar 'is-greatest (list up down left right))))

(defun find-visible-trees (grid)
  (let ((rowcount (length grid))
        (colcount (length (nth 0 grid))))
    (defun check-point (acc cur)
      (cl-destructuring-bind (row col) cur
        (let ((up (getcol grid col 0 row))
              (down (cdr (getcol grid col row rowcount)))
              (left (getrow grid row 0 col))
              (right (cdr (getrow grid row col colcount)))
              (val (getval grid row col)))
          (cond ((edge? up down left right)
                 (append acc (list cur)))
                ((visible? val up down left right)
                 (append acc (list cur)))
                (t acc)))))
    (let* ((rows (number-sequence 0 (- rowcount 1)))
           (cols (number-sequence 0 (- colcount 1)))
           (pairs (make-pairs rows cols)))
      (cl-reduce 'check-point pairs :initial-value '()))))

(defun count-visible-trees (grid)
  (length (find-visible-trees grid)))

(defun count-trees-in-dir (acc cur)
  (cl-destructuring-bind (sum val found) acc
    (cond ((eq t found) acc)
          ((>= cur val)
           (list (+ 1 sum) val t))
          (t (list (+ 1 sum) val nil)))))

(defun count-trees (acc cur)
  (cl-destructuring-bind (sum val) acc
    (let ((total (car (cl-reduce 'count-trees-in-dir cur :initial-value `(0 ,val nil)))))
      (list (append sum (list total)) val))))

(defun find-best-scenic-score (grid)
  (defun score (point)
    (cl-destructuring-bind (row col) point
      (let* ((rowcount (length grid))
             (colcount (length (nth 0 grid)))
             (up (reverse (getcol grid col 0 row)))
             (down (cdr (getcol grid col row rowcount)))
             (left (reverse (getrow grid row 0 col)))
             (right (cdr (getrow grid row col colcount)))
             (val (getval grid row col))
             (res (cl-reduce 'count-trees (list up down left right) :initial-value `(nil ,val))))
        (cl-reduce '* (car res)))))
  (apply 'max (mapcar 'score (find-visible-trees grid))))

(defun make-row (line)
  (mapcar 'string-to-number (split-string line "" t)))

(defsolution part1
  (mapcar 'make-row)
  (count-visible-trees))

(defsolution part2
  (mapcar 'make-row)
  (find-best-scenic-score))

(defsolve "08"
  ((part1 "./08.test.txt") 21)
  ((part1 "./08.input.txt") 1669)
  ((part2 "./08.test.txt") 8)
  ((part2 "./08.input.txt") 331344))
