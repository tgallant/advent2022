;; https://adventofcode.com/2022/day/4

(require 'ert)

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

(defun read-lines (path)
  (with-temp-buffer
    (insert-file-contents path)
    (split-string (buffer-string) "\n" t)))

(defun str-to-range (str)
  (let ((range (mapcar 'string-to-number (split-string str "-" t))))
    (number-sequence (nth 0 range) (nth 1 range))))

(defun str-to-assignments (str)
  (mapcar 'str-to-range (split-string str "," t)))

(ert-deftest 04-count-fully-overlapping-assignments-test-data ()
  (let* ((lines (read-lines "./04.test.txt"))
         (assignments (mapcar 'str-to-assignments lines)))
    (should (= (count-fully-overlapping-assignments assignments) 2))))

(ert-deftest 04-count-fully-overlapping-assignments-input-data ()
  (let* ((lines (read-lines "./04.input.txt"))
         (assignments (mapcar 'str-to-assignments lines)))
    (should (= (count-fully-overlapping-assignments assignments) 509))))

(ert-deftest 04-count-fully-overlapping-assignments-v2-test-data ()
  (let* ((lines (read-lines "./04.test.txt"))
         (assignments (mapcar 'str-to-assignments lines)))
    (should (= (count-fully-overlapping-assignments-v2 assignments) 4))))

(ert-deftest 04-count-fully-overlapping-assignments-v2-input-data ()
  (let* ((lines (read-lines "./04.input.txt"))
         (assignments (mapcar 'str-to-assignments lines)))
    (should (= (count-fully-overlapping-assignments-v2 assignments) 870))))

(ert "04")
