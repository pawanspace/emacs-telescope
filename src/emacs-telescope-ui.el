;;; emacs-telescope-ui.el --- UI components for emacs-telescope -*- lexical-binding: t -*-

;; Copyright (C) 2025 Your Name

;; Author: Your Name <your.email@example.com>
;; Keywords: convenience, files, matching
;; URL: https://github.com/yourusername/emacs-telescope

;;; Commentary:

;; This file provides UI components for emacs-telescope.

;;; Code:

(require 'popup)
(require 'dash)

(defcustom emacs-telescope-ui-border-char ?│
  "Character used for vertical borders."
  :type 'character
  :group 'emacs-telescope)

(defcustom emacs-telescope-ui-header-face 'font-lock-keyword-face
  "Face used for headers in telescope UI."
  :type 'face
  :group 'emacs-telescope)

(defcustom emacs-telescope-ui-selection-face 'highlight
  "Face used for the selected item."
  :type 'face
  :group 'emacs-telescope)

(defcustom emacs-telescope-ui-border-face 'font-lock-comment-face
  "Face used for borders."
  :type 'face
  :group 'emacs-telescope)

(defcustom emacs-telescope-ui-prompt "❯ "
  "Prompt string for telescope input."
  :type 'string
  :group 'emacs-telescope)

(defun emacs-telescope-ui--create-window-layout ()
  "Create the window layout for telescope."
  (let* ((main-window (selected-window))
         (main-buffer (current-buffer))
         (height emacs-telescope-height)
         (width emacs-telescope-width)
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
    
    ;; Return the window configuration
    (list :input-window (selected-window)
          :results-window (get-buffer-window results-buffer)
          :preview-window (get-buffer-window preview-buffer))))

(defun emacs-telescope-ui--setup-input-buffer (title)
  "Setup the input buffer with TITLE."
  (with-current-buffer emacs-telescope--buffer
    (erase-buffer)
    (insert (propertize (concat title "\n") 'face emacs-telescope-ui-header-face))
    (insert emacs-telescope-ui-prompt)
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "C-n") 'emacs-telescope-next-item)
      (define-key map (kbd "C-p") 'emacs-telescope-prev-item)
      (define-key map (kbd "RET") 'emacs-telescope-select-item)
      (define-key map (kbd "C-g") 'emacs-telescope-quit)
      (use-local-map map))
    (setq-local cursor-type 'bar)))

(defun emacs-telescope-ui--setup-results-buffer (title)
  "Setup the results buffer with TITLE."
  (with-current-buffer emacs-telescope--results-buffer
    (erase-buffer)
    (insert (propertize (concat title "\n") 'face emacs-telescope-ui-header-face))
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "C-n") 'emacs-telescope-next-item)
      (define-key map (kbd "C-p") 'emacs-telescope-prev-item)
      (define-key map (kbd "RET") 'emacs-telescope-select-item)
      (define-key map (kbd "C-g") 'emacs-telescope-quit)
      (use-local-map map))
    (setq buffer-read-only t)))

(defun emacs-telescope-ui--setup-preview-buffer (title)
  "Setup the preview buffer with TITLE."
  (with-current-buffer emacs-telescope--preview-buffer
    (erase-buffer)
    (insert (propertize (concat title "\n") 'face emacs-telescope-ui-header-face))
    (setq buffer-read-only t)))

(defun emacs-telescope-ui--update-results (results)
  "Update the results buffer with RESULTS."
  (with-current-buffer emacs-telescope--results-buffer
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (propertize "Results\n" 'face emacs-telescope-ui-header-face))
      (if (null results)
          (insert "No results found")
        (dotimes (i (length results))
          (let ((item (nth i results)))
            (if (= i emacs-telescope--current-selection)
                (insert (propertize (format "> %s\n" item) 'face emacs-telescope-ui-selection-face))
              (insert (format "  %s\n" item)))))))))

(defun emacs-telescope-ui--update-preview (item)
  "Update the preview buffer with ITEM."
  (with-current-buffer emacs-telescope--preview-buffer
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (propertize "Preview\n" 'face emacs-telescope-ui-header-face))
      (cond
       ;; File preview
       ((and (stringp item) (file-exists-p item))
        (insert-file-contents item)
        (let ((mode (assoc-default item auto-mode-alist 'string-match)))
          (when mode (funcall mode))))
       
       ;; Buffer preview
       ((and (stringp item) (get-buffer item))
        (insert-buffer-substring (get-buffer item)))
       
       ;; Default
       (t (insert "No preview available"))))))

(defun emacs-telescope-ui--draw-borders ()
  "Draw borders around telescope windows."
  (let ((border-char (char-to-string emacs-telescope-ui-border-char)))
    ;; Draw borders between windows
    ;; This is a placeholder - actual implementation would depend on how you want to draw borders
    ))

(provide 'emacs-telescope-ui)
;;; emacs-telescope-ui.el ends here
