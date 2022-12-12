;; https://adventofcode.com/2022/day/3

(require 'aoc)

(defun uppercase? (s)
  (let ((case-fold-search nil))
    (not (string-match-p "[[:lower:]]" s))))

(defun calculate-priority (item)
  (let ((char (string-to-char item)))
    (if (uppercase? item) (- char 38)
      (- char 96))))

(defun find-common-item (rucksack)
  (car (cl-intersection (car rucksack) (cdr rucksack) :test 'equal)))

(defun find-common-item-v2 (group)
  (let* ((a (cl-intersection (nth 0 group) (nth 1 group) :test 'equal))
         (b (cl-intersection (nth 1 group) (nth 2 group) :test 'equal)))
    (car (cl-intersection a b :test 'equal))))

(defun calculate-item-priority (rucksacks)
  (let* ((common-items (mapcar 'find-common-item rucksacks))
         (priorities (mapcar 'calculate-priority common-items)))
    (cl-reduce '+ priorities)))

(defun determine-badge-item-type (groups)
  (let* ((badge-types (mapcar 'find-common-item-v2 groups))
         (priorities (mapcar 'calculate-priority badge-types)))
    (cl-reduce '+ priorities)))

(defun str-to-compartments (str)
  (let* ((letters (split-string str "" t))
         (len (length letters))
         (mid (/ len 2))
         (compartment1 (cl-subseq letters 0 mid))
         (compartment2 (cl-subseq letters mid len)))
    (cons compartment1 compartment2)))

(defun lines-to-groups (acc cur)
  (let ((buf (car acc))
        (grp (cdr acc))
        (elf (split-string cur "" t)))
    (if (= 3 (length buf))
        (cons (list elf) (append grp (list buf)))
      (cons (append buf (list elf)) grp))))

(defsolution part1
  (mapcar 'str-to-compartments)
  (calculate-item-priority))

(defsolution part2
  (cl-reduce 'lines-to-groups <> :initial-value '())
  (determine-badge-item-type))

(defsolve "03"
  ((part1 "./03.test.txt") 157)
  ((part1 "./03.input.txt") 8493)
  ((part2 "./03.test.txt") 70)
  ((part2 "./03.input.txt") 2552))
