;;; emacs-telescope-actions.el --- Actions for emacs-telescope -*- lexical-binding: t -*-

;; Copyright (C) 2025 Your Name

;; Author: Your Name <your.email@example.com>
;; Keywords: convenience, files, matching
;; URL: https://github.com/yourusername/emacs-telescope

;;; Commentary:

;; This file provides various actions for emacs-telescope.

;;; Code:

(defun emacs-telescope-action-find-file (file)
  "Open FILE in a buffer."
  (find-file file))

(defun emacs-telescope-action-switch-buffer (buffer)
  "Switch to BUFFER."
  (if (bufferp buffer)
      (switch-to-buffer buffer)
    (switch-to-buffer (get-buffer buffer))))

(defun emacs-telescope-action-grep-goto (grep-result)
  "Go to the location specified by GREP-RESULT."
  (when (string-match "\\(.*\\):\\([0-9]+\\):" grep-result)
    (let ((file (match-string 1 grep-result))
          (line (string-to-number (match-string 2 grep-result))))
      (find-file file)
      (goto-char (point-min))
      (forward-line (1- line)))))

(defun emacs-telescope-action-execute-command (command-name)
  "Execute the command with COMMAND-NAME."
  (call-interactively (intern command-name)))

(defun emacs-telescope-action-git-status-file (status-line)
  "Handle git STATUS-LINE."
  (when (string-match "\\(.\\).\\s-+\\(.*\\)" status-line)
    (let ((status (match-string 1 status-line))
          (file (match-string 2 status-line)))
      (find-file file))))

(defun emacs-telescope-action-insert-at-point (text)
  "Insert TEXT at point."
  (insert text))

(defun emacs-telescope-action-kill-ring-save (text)
  "Save TEXT to kill ring."
  (kill-new text)
  (message "Copied: %s" text))

(defun emacs-telescope-action-open-in-other-window (file)
  "Open FILE in other window."
  (find-file-other-window file))

(defun emacs-telescope-action-open-in-new-frame (file)
  "Open FILE in new frame."
  (find-file-other-frame file))

(defun emacs-telescope-action-dired (directory)
  "Open DIRECTORY in dired."
  (dired directory))

(provide 'emacs-telescope-actions)
;;; emacs-telescope-actions.el ends here
