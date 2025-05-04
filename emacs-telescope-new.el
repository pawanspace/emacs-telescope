;;; emacs-telescope.el --- Fuzzy finder with preview capabilities -*- lexical-binding: t -*-

;; Copyright (C) 2025 Your Name

;; Author: Your Name <your.email@example.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: convenience, files, matching
;; URL: https://github.com/yourusername/emacs-telescope

;;; Commentary:

;; This package provides a fuzzy finder with preview capabilities for Emacs,
;; inspired by telescope.nvim for Neovim.
;;
;; Usage:
;;   M-x emacs-telescope-find-files
;;   M-x emacs-telescope-buffers
;;   M-x emacs-telescope-grep

;;; Code:

(require 'project)
(require 'cl-lib)

(defgroup emacs-telescope nil
  "Fuzzy finder with preview capabilities for Emacs."
  :group 'convenience
  :prefix "emacs-telescope-")

(defcustom emacs-telescope-preview-delay 0.2
  "Delay in seconds before showing preview."
  :type 'number
  :group 'emacs-telescope)

(defcustom emacs-telescope-height 20
  "Height of the telescope window."
  :type 'integer
  :group 'emacs-telescope)

(defcustom emacs-telescope-width 80
  "Width of the telescope window."
  :type 'integer
  :group 'emacs-telescope)

;; Define customization variables for file exclusion
(defcustom emacs-telescope-exclude-dot-files t
  "Whether to exclude dot files (hidden files) from file search results."
  :type 'boolean
  :group 'emacs-telescope)

(defcustom emacs-telescope-exclude-patterns '("\\.git/" "\\.DS_Store$" "\\.elc$")
  "List of regex patterns to exclude from file search results."
  :type '(repeat string)
  :group 'emacs-telescope)

(defvar emacs-telescope--buffer nil
  "Buffer used for telescope input.")

(defvar emacs-telescope--results-buffer nil
  "Buffer used for telescope results.")

(defvar emacs-telescope--preview-buffer nil
  "Buffer used for telescope preview.")

(defvar emacs-telescope--current-source nil
  "Current source for telescope.")

(defvar emacs-telescope--preview-timer nil
  "Timer for delayed preview.")

(defvar emacs-telescope--current-selection 0
  "Current selection index.")

(defvar emacs-telescope--results nil
  "Current results list.")

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

(defun emacs-telescope--create-ui ()
  "Create the telescope UI."
  (let* ((height emacs-telescope-height)
         (input-buffer (get-buffer-create "*telescope-input*"))
         (results-buffer (get-buffer-create "*telescope-results*"))
         (preview-buffer (get-buffer-create "*telescope-preview*")))
    
    ;; Setup buffers
    (setq emacs-telescope--buffer input-buffer)
    (setq emacs-telescope--results-buffer results-buffer)
    (setq emacs-telescope--preview-buffer preview-buffer)
    
    ;; Create windows
    (delete-other-windows)
    (split-window-vertically (- (window-height) height 1))
    (other-window 1)
    (split-window-horizontally)
    
    ;; Set buffers in windows
    (set-window-buffer (selected-window) results-buffer)
    (other-window 1)
    (set-window-buffer (selected-window) preview-buffer)
    (other-window -1)
    (split-window-vertically 1)
    (set-window-buffer (selected-window) input-buffer)
    
    ;; Setup input buffer
    (with-current-buffer input-buffer
      (erase-buffer)
      (insert "Type to search...")
      (local-set-key (kbd "C-n") 'emacs-telescope-next-item)
      (local-set-key (kbd "C-p") 'emacs-telescope-prev-item)
      (local-set-key (kbd "RET") 'emacs-telescope-select-item)
      (local-set-key (kbd "C-g") 'emacs-telescope-quit))
    
    ;; Setup results buffer
    (with-current-buffer results-buffer
      (erase-buffer))
    
    ;; Setup preview buffer
    (with-current-buffer preview-buffer
      (erase-buffer))))

(defun emacs-telescope-next-item ()
  "Select next item in telescope."
  (interactive)
  (when (< emacs-telescope--current-selection (1- (length emacs-telescope--results)))
    (cl-incf emacs-telescope--current-selection)
    (emacs-telescope--update-selection)))

(defun emacs-telescope-prev-item ()
  "Select previous item in telescope."
  (interactive)
  (when (> emacs-telescope--current-selection 0)
    (cl-decf emacs-telescope--current-selection)
    (emacs-telescope--update-selection)))

(defun emacs-telescope--update-selection ()
  "Update the selection in the results buffer."
  (with-current-buffer emacs-telescope--results-buffer
    (let ((inhibit-read-only t))
      (erase-buffer)
      (dotimes (i (length emacs-telescope--results))
        (let ((item (nth i emacs-telescope--results)))
          (if (= i emacs-telescope--current-selection)
              (insert (propertize (format "> %s\n" item) 'face 'highlight))
            (insert (format "  %s\n" item)))))
      (emacs-telescope--update-preview))))

(defun emacs-telescope--update-preview ()
  "Update the preview based on current selection."
  (when emacs-telescope--preview-timer
    (cancel-timer emacs-telescope--preview-timer))
  
  (setq emacs-telescope--preview-timer
        (run-with-timer
         emacs-telescope-preview-delay nil
         (lambda ()
           (when (and emacs-telescope--results
                     (>= emacs-telescope--current-selection 0)
                     (< emacs-telescope--current-selection (length emacs-telescope--results)))
             (let ((selected (nth emacs-telescope--current-selection emacs-telescope--results)))
               (with-current-buffer emacs-telescope--preview-buffer
                 (let ((inhibit-read-only t))
                   (erase-buffer)
                   (cond
                    ;; Grep result preview
                    ((string-match "\\(.*\\):\\([0-9]+\\):" selected)
                     (let ((file (match-string 1 selected))
                           (line (string-to-number (match-string 2 selected))))
                       (if (file-exists-p file)
                           (progn
                             (insert-file-contents file)
                             (let ((mode (assoc-default file auto-mode-alist 'string-match)))
                               (when mode (funcall mode)))
                             ;; Highlight the matching line
                             (goto-char (point-min))
                             (forward-line (1- line))
                             (let ((start (line-beginning-position))
                                   (end (line-end-position)))
                               (put-text-property start end 'face 'highlight)))
                         (insert (format "File not found: %s" file)))))
                    
                    ;; File preview
                    ((and (stringp selected) (file-exists-p selected))
                     (insert-file-contents selected)
                     (let ((mode (assoc-default selected auto-mode-alist 'string-match)))
                       (when mode (funcall mode))))
                    
                    ;; Buffer preview
                    ((and (stringp selected) (get-buffer selected))
                     (insert-buffer-substring (get-buffer selected)))
                    
                    ;; Default
                    (t (insert "No preview available"))))))))))

(defun emacs-telescope-select-item ()
  "Select the current item."
  (interactive)
  (when (and emacs-telescope--results
             (>= emacs-telescope--current-selection 0)
             (< emacs-telescope--current-selection (length emacs-telescope--results)))
    (let ((selected (nth emacs-telescope--current-selection emacs-telescope--results)))
      (emacs-telescope-quit)
      (cond
       ;; Grep result
       ((string-match "\\(.*\\):\\([0-9]+\\):" selected)
        (let ((file (match-string 1 selected))
              (line (string-to-number (match-string 2 selected))))
          (find-file file)
          (goto-char (point-min))
          (forward-line (1- line))))
       
       ;; File selection
       ((and (stringp selected) (file-exists-p selected))
        (find-file selected))
       
       ;; Buffer selection
       ((and (stringp selected) (get-buffer selected))
        (switch-to-buffer (get-buffer selected)))))))

(defun emacs-telescope-quit ()
  "Quit telescope."
  (interactive)
  (when emacs-telescope--preview-timer
    (cancel-timer emacs-telescope--preview-timer))
  
  (when (buffer-live-p emacs-telescope--buffer)
    (kill-buffer emacs-telescope--buffer))
  
  (when (buffer-live-p emacs-telescope--results-buffer)
    (kill-buffer emacs-telescope--results-buffer))
  
  (when (buffer-live-p emacs-telescope--preview-buffer)
    (kill-buffer emacs-telescope--preview-buffer))
  
  (delete-other-windows))

(defun emacs-telescope--filter-results (query items)
  "Filter ITEMS based on QUERY."
  (if (string-empty-p query)
      items
    (let ((query-terms (split-string query " " t)))
      (seq-filter
       (lambda (item)
         (let ((item-str (if (stringp item) item (format "%s" item))))
           (seq-every-p
            (lambda (term)
              (string-match-p (regexp-quote term) item-str))
            query-terms)))
       items))))

;;;###autoload
(defun emacs-telescope-find-files ()
  "Find files using telescope."
  (interactive)
  (let* ((project-root (project-root (project-current t)))
         (default-directory project-root)
         (all-files (directory-files-recursively project-root ".*" nil))
         (files (seq-filter (lambda (f) (not (emacs-telescope--should-exclude-file-p f))) all-files)))
    (setq emacs-telescope--results files)
    (setq emacs-telescope--current-selection 0)
    (emacs-telescope--create-ui)
    (emacs-telescope--update-selection)))

;;;###autoload
(defun emacs-telescope-buffers ()
  "Find buffers using telescope."
  (interactive)
  (let* ((buffers (mapcar #'buffer-name (buffer-list))))
    (setq emacs-telescope--results buffers)
    (setq emacs-telescope--current-selection 0)
    (emacs-telescope--create-ui)
    (emacs-telescope--update-selection)))

;;;###autoload
(defun emacs-telescope-grep ()
  "Grep in project using telescope."
  (interactive)
  (let* ((project-root (project-root (project-current t)))
         (default-directory project-root)
         (query (read-string "Grep for: "))
         (grep-cmd (format "grep -r \"%s\" --include=\"*.el\" --include=\"*.py\" --include=\"*.js\" . 2>/dev/null" query))
         (results (split-string (shell-command-to-string grep-cmd) "\n" t)))
    (setq emacs-telescope--results results)
    (setq emacs-telescope--current-selection 0)
    (emacs-telescope--create-ui)
    (emacs-telescope--update-selection)))

(provide 'emacs-telescope)
;;; emacs-telescope.el en
