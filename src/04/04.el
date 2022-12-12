;; https://adventofcode.com/2022/day/4

(require 'aoc)

(defun determine-if-overlapping (acc cur)
  (let* ((left (nth 0 cur))
         (right (nth 1 cur))
         (l-intersection (cl-intersection left right))
         (r-intersection (cl-intersection right left)))
    (cond ((eq (length left) (length l-intersection))
           (+ acc 1))
          ((eq (length right) (length r-intersection))
           (+ acc 1))
          (acc))))

(defun determine-if-overlapping-v2 (acc cur)
  (let* ((left (nth 0 cur))
         (right (nth 1 cur))
         (l-intersection (cl-intersection left right))
         (r-intersection (cl-intersection right left)))
    (cond ((> (length l-intersection) 0)
           (+ acc 1))
          ((> (length r-intersection) 0)
           (+ acc 1))
          (acc))))

(defun count-fully-overlapping-assignments (assignments)
  (cl-reduce 'determine-if-overlapping assignments :initial-value 0))

(defun count-fully-overlapping-assignments-v2 (assignments)
  (cl-reduce 'determine-if-overlapping-v2 assignments :initial-value 0))

(defun str-to-range (str)
  (let ((range (mapcar 'string-to-number (split-string str "-" t))))
    (number-sequence (nth 0 range) (nth 1 range))))

(defun str-to-assignments (str)
  (mapcar 'str-to-range (split-string str "," t)))

(defsolution part1
  (mapcar 'str-to-assignments)
  (count-fully-overlapping-assignments))

(defsolution part2
  (mapcar 'str-to-assignments)
  (count-fully-overlapping-assignments-v2))

(defsolve "04"
  ((part1 "./04.test.txt") 2)
  ((part1 "./04.input.txt") 509)
  ((part2 "./04.test.txt") 4)
  ((part2 "./04.input.txt") 870))
