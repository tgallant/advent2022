;; https://adventofcode.com/2022/day/2

(require 'ert)

(defconst move-mapping '(A rock B paper C scissors X rock Y paper Z scissors))
(defconst outcome-mapping '(X lose Y draw Z win))

(defun determine-winner (round)
  (let ((p1 (car round))
        (p2 (cdr round)))
    (cond ((eq p1 p2) 'draw)
          ((and (eq p1 'rock) (eq p2 'scissors)) 'p1)
          ((and (eq p1 'scissors) (eq p2 'paper)) 'p1)
          ((and (eq p1 'paper) (eq p2 'rock)) 'p1)
          ('p2))))

(defun score-move (move)
  (cond ((eq move 'rock) 1)
        ((eq move 'paper) 2)
        ((eq move 'scissors) 3)))

(defun score (acc cur)
  (let ((result (determine-winner cur))
        (p2-move-score (score-move (cdr cur))))
    (cond ((eq result 'p1)
           (+ acc p2-move-score))
          ((eq result 'draw)
           (+ acc p2-move-score 3))
          ((eq result 'p2)
           (+ acc p2-move-score 6)))))

(defun calculate-rpc-score (rounds)
  (cl-reduce 'score rounds :initial-value 0))

(defun read-lines (path)
  (with-temp-buffer
    (insert-file-contents path)
    (split-string (buffer-string) "\n" t)))

(defun decrypt (move)
  (plist-get move-mapping (intern move)))

(defun decrypt-p2 (move)
  (plist-get outcome-mapping (intern move)))

(defun str-to-round (str)
  (let* ((moves (split-string str))
         (player1 (decrypt (nth 0 moves)))
         (player2 (decrypt (nth 1 moves))))
    (cons player1 player2)))

(defun determine-move (move outcome)
  (cond ((eq outcome 'draw) move)
        ((and (eq outcome 'win) (eq move 'rock)) 'paper)
        ((and (eq outcome 'win) (eq move 'paper)) 'scissors)
        ((and (eq outcome 'win) (eq move 'scissors)) 'rock)
        ((and (eq outcome 'lose) (eq move 'rock)) 'scissors)
        ((and (eq outcome 'lose) (eq move 'paper)) 'rock)
        ((and (eq outcome 'lose) (eq move 'scissors)) 'paper)))

(defun str-to-round-v2 (str)
  (let* ((moves (split-string str))
         (player1 (decrypt (nth 0 moves)))
         (outcome (decrypt-p2 (nth 1 moves)))
         (player2 (determine-move player1 outcome)))
    (cons player1 player2)))

(ert-deftest 02-calculate-rpc-scores-test-data ()
  (let* ((lines (read-lines "./02.test.txt"))
         (rounds (mapcar 'str-to-round lines)))
    (should (= (calculate-rpc-score rounds) 15))))

(ert-deftest 02-calculate-rpc-scores-input-data ()
  (let* ((lines (read-lines "./02.input.txt"))
         (rounds (mapcar 'str-to-round lines)))
    (should (= (calculate-rpc-score rounds) 12772))))

(ert-deftest 02-calculate-rpc-scores-v2-test-data ()
  (let* ((lines (read-lines "./02.test.txt"))
         (rounds (mapcar 'str-to-round-v2 lines)))
    (should (= (calculate-rpc-score rounds) 12))))

(ert-deftest 02-calculate-rpc-scores-v2-input-data ()
  (let* ((lines (read-lines "./02.input.txt"))
         (rounds (mapcar 'str-to-round-v2 lines)))
    (should (= (calculate-rpc-score rounds) 11618))))

(ert "02")
