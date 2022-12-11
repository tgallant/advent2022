;; https://adventofcode.com/2022/day/6

(require 'aoc)
(require 'seq)

(defun make-buf (buf el len)
  (if (< (length buf) len) (append buf (list cur))
    (append (cl-subseq buf 1) (list cur))))

(defun check-packet (acc cur)
  (cl-destructuring-bind (pos buf found len) acc
    (let ((newbuf (make-buf buf cur len)))
      (cond ((eq found t) acc)
            ((and (= len (length newbuf)) (= len (length (seq-uniq newbuf))))
             (list (+ pos 1) newbuf t len))
            (t (list (+ pos 1) newbuf nil len))))))

(defun find-start-of-packet-marker (packet)
  (car (cl-reduce 'check-packet packet :initial-value '(0 nil nil 4))))

(defun find-start-of-message-marker (packet)
  (car (cl-reduce 'check-packet packet :initial-value '(0 nil nil 14))))

(defun extract-packet (lines)
  (split-string (nth 0 lines) "" t))

(defsolution part1
  (extract-packet)
  (find-start-of-packet-marker))

(defsolution part2
  (extract-packet)
  (find-start-of-message-marker))

(defsolve "06"
 ((part1 "./06.test.txt") 7)
 ((part1 "./06.input.txt") 1625)
 ((part2 "06.test.txt") 19)
 ((part2 "06.input.txt") 2250))
