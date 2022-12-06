;; https://adventofcode.com/2022/day/6

(require 'ert)
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

(defun read-lines (path)
  (with-temp-buffer
    (insert-file-contents path)
    (split-string (buffer-string) "\n" t)))

(ert-deftest 06-find-start-of-packet-marker-test-data ()
  (let* ((lines (read-lines "./06.test.txt"))
         (packet (split-string (nth 0 lines) "" t)))
    (should (= (find-start-of-packet-marker packet) 7))))

(ert-deftest 06-find-start-of-packet-marker-input-data ()
  (let* ((lines (read-lines "./06.input.txt"))
         (packet (split-string (nth 0 lines) "" t)))
    (should (= (find-start-of-packet-marker packet) 1625))))

(ert-deftest 06-find-start-of-message-marker-test-data ()
  (let* ((lines (read-lines "./06.test.txt"))
         (packet (split-string (nth 0 lines) "" t)))
    (should (= (find-start-of-message-marker packet) 19))))

(ert-deftest 06-find-start-of-message-marker-input-data ()
  (let* ((lines (read-lines "./06.input.txt"))
         (packet (split-string (nth 0 lines) "" t)))
    (should (= (find-start-of-message-marker packet) 2250))))

(ert "06")
