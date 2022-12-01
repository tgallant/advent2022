;; https://adventofcode.com/2022/day/1

(require 'ert)

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

(defun read-lines (path)
  (with-temp-buffer
    (insert-file-contents path)
    (split-string (buffer-string) "\n")))

(defun str-to-num (val)
  (if (equal val "") ""
    (string-to-number val)))

(ert-deftest 01-max-calories-test-data ()
  (let* ((lines (read-lines "./01.test.txt"))
         (vals (mapcar 'str-to-num lines)))
    (should (= (max-calories vals) 24000))))

(ert-deftest 01-max-calories-input-data ()
  (let* ((lines (read-lines "./01.input.txt"))
         (vals (mapcar 'str-to-num lines)))
    (should (= (max-calories vals) 67016))))

(ert-deftest 01-top-three-calories-test-data ()
  (let* ((lines (read-lines "./01.test.txt"))
         (vals (mapcar 'str-to-num lines)))
    (should (= (top-three-calories vals) 45000))))

(ert-deftest 01-top-three-calories-input-data ()
  (let* ((lines (read-lines "./01.input.txt"))
         (vals (mapcar 'str-to-num lines)))
    (should (= (top-three-calories vals) 200116))))

(ert "01")
