;; https://adventofcode.com/2022/day/7

(require 'ert)
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

(defun read-lines (path)
  (with-temp-buffer
    (insert-file-contents path)
    (split-string (buffer-string) "\n" t)))

(ert-deftest 07-sum-directory-sizes-test-data ()
  (let* ((lines (read-lines "./07.test.txt")))
    (should (= (sum-directory-sizes lines) 95437))))

(ert-deftest 07-sum-directory-sizes-input-data ()
  (let* ((lines (read-lines "./07.input.txt")))
    (should (= (sum-directory-sizes lines) 1778099))))

(ert-deftest 07-smallest-dir-to-delete-test-data ()
  (let* ((lines (read-lines "./07.test.txt")))
    (should (= (smallest-dir-to-delete lines) 24933642))))

(ert-deftest 07-smallest-dir-to-delete-input-data ()
  (let* ((lines (read-lines "./07.input.txt")))
    (should (= (smallest-dir-to-delete lines) 1623571))))

(ert "07")
