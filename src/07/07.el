;; https://adventofcode.com/2022/day/7

(require 'aoc)
(require 'subr-x)

(defun add-size-to-dirs (size dirs stack)
  (defun add (cur acc)
    (cl-destructuring-bind (dirs path) acc
      (let* ((p (string-join (cons cur path) "/"))
             (v (intern p)))
        (list (plist-put dirs v (+ size (or (plist-get dirs v) 0))) (cons cur path)))))
  (car (cl-reduce 'add stack :initial-value (list dirs nil) :from-end t)))

(defun calculate-directory-sizes (acc cur)
  (cl-destructuring-bind (dirs stack) acc
    (let ((output (split-string cur)))
      (pcase output
        (`("$" "cd" "/")
         (list dirs (cons "/" stack)))
        (`("$" "cd" "..")
         (list dirs (cdr stack)))
        (`("$" "cd" ,dir)
         (list dirs (cons dir stack)))
        (`("$" "ls")
         (list dirs stack))
        (`("dir" ,_)
         (list dirs stack))
        (`(,size ,_)
         (list (add-size-to-dirs (string-to-number size) dirs stack) stack))))))

(defun sum-dirs (acc cur)
  (cl-destructuring-bind (sum dir) acc
    (cond ((symbolp cur)
           (list sum cur))
          ((and (numberp cur) (<= cur 100000))
           (list (+ sum cur) dir))
          (t (list sum dir)))))

(defun sum-directory-sizes (lines)
  (let ((sizes (car (cl-reduce 'calculate-directory-sizes lines :initial-value '(nil nil)))))
    (car (cl-reduce 'sum-dirs sizes :initial-value '(0 nil)))))

(defun find-smallest-dir (acc cur)
  (cl-destructuring-bind (val target) acc
    (cond ((symbolp cur)
           (list val target))
          ((and (numberp cur) (>= cur target) (< cur val))
           (list cur target))
          (t (list val target)))))

(defun smallest-dir-to-delete (lines)
  (let* ((sizes (car (cl-reduce 'calculate-directory-sizes lines :initial-value '(nil nil))))
         (root (plist-get sizes '/))
         (free (- 70000000 root))
         (diff (- 30000000 free)))
    (car (cl-reduce 'find-smallest-dir sizes :initial-value `(1.0e+INF ,diff)))))

(defsolution part1
  (sum-directory-sizes))

(defsolution part2
  (smallest-dir-to-delete))

(defsolve "07"
  ((part1 "./07.test.txt") 95437)
  ((part1 "./07.input.txt") 1778099)
  ((part2 "./07.test.txt") 24933642)
  ((part2 "./07.input.txt") 1623571))
