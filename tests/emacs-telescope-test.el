;;; emacs-telescope-test.el --- Tests for emacs-telescope -*- lexical-binding: t -*-

;; Copyright (C) 2025 Your Name

;;; Commentary:

;; Tests for emacs-telescope.

;;; Code:

(require 'ert)
(require 'emacs-telescope)

(ert-deftest emacs-telescope-test-filter-results ()
  "Test the filtering function."
  (let ((items '("apple" "banana" "cherry" "date" "elderberry")))
    ;; Empty query returns all items
    (should (equal (emacs-telescope--filter-results "" items) items))
    
    ;; Single term filtering
    (should (equal (emacs-telescope--filter-results "a" items)
                   '("apple" "banana" "date" "elderberry")))
    
    ;; Multiple term filtering (AND logic)
    (should (equal (emacs-telescope--filter-results "a e" items)
                   '("elderberry")))))

(ert-deftest emacs-telescope-test-selection ()
  "Test selection functions."
  (let ((emacs-telescope--results '("item1" "item2" "item3"))
        (emacs-telescope--current-selection 0))
    
    ;; Test next item
    (emacs-telescope-next-item)
    (should (= emacs-telescope--current-selection 1))
    
    ;; Test previous item
    (emacs-telescope-prev-item)
    (should (= emacs-telescope--current-selection 0))
    
    ;; Test bounds
    (emacs-telescope-prev-item) ; Should not go below 0
    (should (= emacs-telescope--current-selection 0))
    
    (setq emacs-telescope--current-selection 2)
    (emacs-telescope-next-item) ; Should not go above length - 1
    (should (= emacs-telescope--current-selection 2))))

(provide 'emacs-telescope-test)
;;; emacs-telescope-test.el ends here
