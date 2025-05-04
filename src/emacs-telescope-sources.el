;;; emacs-telescope-sources.el --- Sources for emacs-telescope -*- lexical-binding: t -*-

;;; Commentary:

;; This file provides various sources for emacs-telescope.

;;; Code:

(require 'project)
(require 'dash)

(defcustom emacs-telescope-exclude-dot-files t
  "Whether to exclude dot files (hidden files) from file search results."
  :type 'boolean
  :group 'emacs-telescope)

(defcustom emacs-telescope-exclude-patterns '("\\.git/" "\\.DS_Store$" "\\.elc$")
  "List of regex patterns to exclude from file search results."
  :type '(repeat string)
  :group 'emacs-telescope)

(defun emacs-telescope--should-exclude-file-p (file)
  "Return non-nil if FILE should be excluded from results."
  (let ((relative-file (file-relative-name file)))
    (or
     ;; Exclude dot files if configured
     (and emacs-telescope-exclude-dot-files
          (string-match-p "/\\.[^/]+" (concat "/" relative-file)))
     ;; Exclude files matching patterns
     (seq-some (lambda (pattern)
                 (string-match-p pattern relative-file))
               emacs-telescope-exclude-patterns))))

(defun emacs-telescope-source-files ()
  "Source for finding files in the current project."
  (let* ((project-root (project-root (project-current t)))
         (default-directory project-root)
         (all-files (directory-files-recursively project-root ".*" nil)))
    ;; Filter out excluded files
    (-remove #'emacs-telescope--should-exclude-file-p all-files)))

(defun emacs-telescope-source-buffers ()
  "Source for finding open buffers."
  (let ((buffers (buffer-list)))
    (-filter (lambda (buf)
               (let ((name (buffer-name buf)))
                 (and (not (string-prefix-p " " name))
                      (not (string-prefix-p "*" name)))))
             buffers)))

(defun emacs-telescope-source-recent-files ()
  "Source for finding recently opened files."
  (require 'recentf)
  (recentf-mode 1)
  recentf-list)

(defun emacs-telescope-source-grep (query)
  "Source for grep results with QUERY."
  (require 'emacs-telescope-grep)
  (emacs-telescope-grep-get-results query))

(defun emacs-telescope-source-git-files ()
  "Source for git files in the current project."
  (let* ((project-root (project-root (project-current t)))
         (default-directory project-root)
         (git-cmd "git ls-files")
         (results (split-string (shell-command-to-string git-cmd) "\n" t)))
    results))

(defun emacs-telescope-source-git-status ()
  "Source for git status in the current project."
  (let* ((project-root (project-root (project-current t)))
         (default-directory project-root)
         (git-cmd "git status --porcelain")
         (results (split-string (shell-command-to-string git-cmd) "\n" t)))
    results))

(defun emacs-telescope-source-commands ()
  "Source for Emacs commands."
  (let ((cmds nil))
    (mapatoms
     (lambda (sym)
       (when (and (commandp sym)
                  (not (string-prefix-p "ad-Orig" (symbol-name sym)))
                  (not (string-prefix-p "ad-Advice" (symbol-name sym))))
         (push (symbol-name sym) cmds))))
    cmds))

(provide 'emacs-telescope-sources)
;;; emacs-telescope-sources.el ends here
