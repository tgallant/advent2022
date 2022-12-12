;; https://adventofcode.com/2022/day/5

(require 'aoc)
(require 'subr-x)

(defun rearrange-stacks (stacks moves)
  (defun step (m)
    (let ((move (plist-get m 'move))
          (from (plist-get m 'from))
          (to (plist-get m 'to)))
      (dotimes (n move)
        (let* ((from-val (gethash from stacks))
               (to-val (gethash to stacks))
               (next-val (cons (car from-val) to-val)))
          (puthash from (cdr from-val) stacks)
          (puthash to next-val stacks)))))
  (mapc 'step moves)
  (string-join (mapcar 'car (hash-table-values stacks))))

(defun rearrange-stacks-v2 (stacks moves)
  (defun step (m)
    (let* ((move (plist-get m 'move))
           (from (plist-get m 'from))
           (to (plist-get m 'to))
           (from-val (cl-subseq (gethash from stacks) 0 move))
           (to-val (gethash to stacks))
           (next-from-val (cl-subseq (gethash from stacks) move))
           (next-to-val (append from-val to-val)))
      (puthash from next-from-val stacks)
      (puthash to next-to-val stacks)))
  (mapc 'step moves)
  (string-join (mapcar 'car (hash-table-values stacks))))

(defun lines-to-stacks-and-moves (acc cur)
  (let ((stacks (nth 0 acc))
        (moves (nth 1 acc))
        (seen-newline? (nth 2 acc)))
    (cond ((equal cur "")
           (list stacks moves t))
          ((eq t seen-newline?)
           (list stacks (append moves (list cur)) seen-newline?))
          ((list (append stacks (list cur)) moves seen-newline?)))))

(defun parse-stack-str (acc cur)
  (let ((pos (nth 0 acc))
        (count (nth 1 acc))
        (stacks (nth 2 acc)))
    (cond ((eq count 1)
           (let ((val (if (equal cur " ") nil cur)))
             (puthash pos val stacks)
             (list pos (+ count 1) stacks)))
          ((eq count 3)
           (list (+ pos 1) 0 stacks))
          (t (list pos (+ count 1) stacks)))))

(defun parse-stacks (str)
  (let ((chars (split-string str "" t))
        (ht (make-hash-table)))
    (nth 2 (cl-reduce 'parse-stack-str chars :initial-value (list 1 0 ht)))))

(defun merge-stacks (src trg)
  (defun merge (key val)
    (if (not (eq val nil))
        (puthash key (cons val (gethash key trg)) trg)))
  (maphash 'merge src)
  trg)

(defun build-stacks (cur acc)
  (if (eq cur nil) acc
    (merge-stacks (parse-stacks cur) acc)))

(defun make-stacks (lines)
  (let ((ht (make-hash-table))
        (stack-lines (butlast lines)))
    (cl-reduce 'build-stacks stack-lines :initial-value ht :from-end t)))

(defun symbol-or-number (val)
  (if (or (> (length val) 2) (equal val "to"))
      (intern val)
    (string-to-number val)))

(defun build-move (str)
  (mapcar 'symbol-or-number (split-string str)))

(defun make-moves (lines)
  (mapcar 'build-move lines))

(defun make-stacks-and-moves (lines)
  (let ((res (cl-reduce 'lines-to-stacks-and-moves lines :initial-value '())))
    (list (make-stacks (nth 0 res)) (make-moves (nth 1 res)))))

(defsolution-with-null part1
  (make-stacks-and-moves)
  (apply 'rearrange-stacks))

(defsolution-with-null part2
  (make-stacks-and-moves)
  (apply 'rearrange-stacks-v2))

(defsolve "05"
  ((part1 "./05.test.txt") "CMZ")
  ((part1 "./05.input.txt") "LBLVVTVLP")
  ((part2 "./05.test.txt") "MCD")
  ((part2 "./05.input.txt") "TPFFBDRJD"))
