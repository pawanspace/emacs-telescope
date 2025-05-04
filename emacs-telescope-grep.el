;;; emacs-telescope-grep.el --- Grep functionality for emacs-telescope -*- lexical-binding: t -*-

;; Copyright (C) 2025 Your Name

;; Author: Your Name <your.email@example.com>
;; Keywords: convenience, files, matching
;; URL: https://github.com/yourusername/emacs-telescope

;;; Commentary:

;; This file provides grep functionality for emacs-telescope.

;;; Code:

(require 'project)

(defcustom emacs-telescope-grep-file-types
  '("*.el" "*.py" "*.js" "*.java" "*.c" "*.cpp" "*.rs" "*.html" "*.css" "*.md")
  "List of file types to include in grep searches."
  :type '(repeat string)
  :group 'emacs-telescope)

(defun emacs-telescope-grep-get-results (query)
  "Get grep results for QUERY in the current project.
Returns a list of matching lines."
  (let* ((project-root (project-root (project-current t)))
         (default-directory project-root)
         (include-args (mapconcat (lambda (type) (format "--include=\"%s\"" type))
                                 emacs-telescope-grep-file-types
                                 " "))
         (grep-cmd (format "grep -r \"%s\" %s . 2>/dev/null" query include-args))
         (results (split-string (shell-command-to-string grep-cmd) "\n" t)))
    results))

;;;###autoload
(defun emacs-telescope-grep ()
  "Grep in project using telescope."
  (interactive)
  (let* ((query (read-string "Grep for: "))
         (results (emacs-telescope-grep-get-results query)))
    (setq emacs-telescope--results results)
    (setq emacs-telescope--current-selection 0)
    (emacs-telescope--create-ui)
    (emacs-telescope--update-selection)))

(provide 'emacs-telescope-grep)
;;; emacs-telescope-grep.el ends here
