;;; emacs-telescope-grep.el --- Grep functionality for emacs-telescope -*- lexical-binding: t -*-

;; Copyright (C) 2025 Your Name

;; Author: Your Name <your.email@example.com>
;; Keywords: convenience, files, matching
;; URL: https://github.com/yourusername/emacs-telescope

;;; Commentary:

;; This file provides grep functionality for emacs-telescope.

;;; Code:

(require 'project)

;;;###autoload
(defun emacs-telescope-grep ()
  "Grep in project using telescope."
  (interactive)
  (let* ((project-root (project-root (project-current t)))
         (default-directory project-root)
         (query (read-string "Grep for: "))
         (grep-cmd (format "grep -r \"%s\" --include=\"*.el\" --include=\"*.py\" --include=\"*.js\" --include=\"*.java\" --include=\"*.c\" --include=\"*.cpp\" --include=\"*.rs\" . 2>/dev/null" query))
         (results (split-string (shell-command-to-string grep-cmd) "\n" t)))
    (setq emacs-telescope--results results)
    (setq emacs-telescope--current-selection 0)
    (emacs-telescope--create-ui)
    (emacs-telescope--update-selection)))

(provide 'emacs-telescope-grep)
;;; emacs-telescope-grep.el ends here
