;;; emacs-telescope.el --- Fuzzy finder with preview capabilities -*- lexical-binding: t -*-

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
(require 'emacs-telescope-grep)
(require 'emacs-telescope-ui)

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

(defvar emacs-telescope--current-query nil
  "The query used for the current telescope session, if applicable.")

(defvar emacs-telescope--original-results nil
  "The original, unfiltered results list for the current session.")


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



(defun emacs-telescope--filter-on-input-change (_beg _end _len)
  "Filter results based on input buffer changes. Hook function."
  ;; Prevent recursive calls and calls during programmatic changes
  (unless inhibit-modification-hooks
     (message "Filter hook triggered...") ; Uncomment for debugging
    (let ((query "") ;; Default to empty query
          (prompt-found-p nil))
      ;; Safely find the end of the prompt
      (save-excursion
        (goto-char (point-min))
        (when (search-forward emacs-telescope-ui-prompt nil t) ; Search non-destructively
          (setq query (buffer-substring-no-properties (point) (point-max)))
          (setq prompt-found-p t)))

      ;; Only filter if the prompt was found (sanity check)
      (if prompt-found-p
          (progn
            ;; (message "Filtering with query: %s" query) ; Uncomment for debugging
            (let ((filtered (emacs-telescope--filter-results query emacs-telescope--original-results)))
              ;; Update the displayed results
              (setq emacs-telescope--results filtered)
              ;; Reset selection to the top
              (setq emacs-telescope--current-selection 0)
              ;; Update the results buffer display and preview via timer
              (when emacs-telescope--filter-timer (cancel-timer emacs-telescope--filter-timer))
              (setq emacs-telescope--filter-timer
                    (run-with-timer 0.05 nil #'emacs-telescope--update-selection))))
         (message "Prompt not found in input buffer!") ; Uncomment for debugging
        ))))


;; Add a timer variable for debouncing filter updates
(defvar emacs-telescope--filter-timer nil
  "Timer for debouncing filter updates.")


(defun emacs-telescope--create-ui ()
  "Create the telescope UI."
  (let* ((height emacs-telescope-height)
         (input-buffer (get-buffer-create "*telescope-input*"))
         (results-buffer (get-buffer-create "*telescope-results*"))
         (preview-buffer (get-buffer-create "*telescope-preview*"))
         ;; Define border characters and face
         (border-char-h ?─) ; Horizontal line character (Box Drawings Light Horizontal)
         (border-face emacs-telescope-ui-border-face)) ; Use the face defined in ui.el

    ;; Setup global buffer variables
    (setq emacs-telescope--buffer input-buffer)
    (setq emacs-telescope--results-buffer results-buffer)
    (setq emacs-telescope--preview-buffer preview-buffer)

    ;; --- Create window layout ---
    (delete-other-windows)
    (when (< height (frame-height))
        (split-window-vertically (- (window-height) height 1)))
    (other-window 1) ; Move to the bottom (telescope) window
    (split-window-horizontally) ; Default 50/50 split

    ;; Assign results and preview buffers AND define window vars for later use
    (let* ((results-window (selected-window))
           (preview-window (next-window))
           ;; Define input-window here, initially nil
           (input-window nil))
      (set-window-buffer results-window results-buffer)
      (set-window-buffer preview-window preview-buffer)

      ;; Split results window vertically for input buffer (1 line high)
      (select-window results-window)
      (split-window-vertically 1)
      ;; Now set the input-window variable defined in the outer let
      (setq input-window (selected-window))
      (set-window-buffer input-window input-buffer)

      ;; --- Setup Input Buffer ---
      (with-selected-window input-window
        (with-current-buffer input-buffer
          (erase-buffer)
          ;; Add Header Line (Top Border for Input)
          (let ((width (max 1 (window-body-width))))
            (setq header-line-format
                  (propertize (make-string width border-char-h) 'face border-face)))
          ;; Insert Prompt
          (insert emacs-telescope-ui-prompt)
          (goto-char (point-max))
          ;; Set Keymap
          (local-set-key (kbd "C-n") 'emacs-telescope-next-item)
          (local-set-key (kbd "C-p") 'emacs-telescope-prev-item)
          (local-set-key (kbd "RET") 'emacs-telescope-select-item)
          (local-set-key (kbd "C-g") 'emacs-telescope-quit)
          ))

      ;; --- Setup Results Buffer ---
      (select-window (next-window results-window)) ; Select the window below input
      (with-selected-window (selected-window) ; Now this is the results window
          (with-current-buffer results-buffer
            (erase-buffer)
            ;; Add Header Line (Top Border for Results)
            (let ((width (max 1 (window-body-width))))
              (setq header-line-format
                    (propertize (make-string width border-char-h) 'face border-face)))
            (setq buffer-read-only t)))

      ;; --- Setup Preview Buffer ---
      (select-window preview-window)
      (with-selected-window preview-window
          (with-current-buffer preview-buffer
            (erase-buffer)
            ;; Optional: Add header line to preview too for consistency
            (let ((width (max 1 (window-body-width))))
              (setq header-line-format
                    (propertize (make-string width border-char-h) 'face border-face)))
            (setq buffer-read-only t)))

      ;; --- Final Setup ---
      ;; Reset input state tracking and add post-command-hook
      (setq emacs-telescope--last-input-tick -1)
      (setq emacs-telescope--last-input-length -1)
      (add-hook 'post-command-hook #'emacs-telescope--check-input-change-via-post-command)

      ;; Select the input window initially for typing
      ;; Now input-window is accessible here
      (select-window input-window))))



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
  "Update the selection in the results buffer and trigger preview."
  (when (and emacs-telescope--results-buffer (buffer-live-p emacs-telescope--results-buffer))
    (with-current-buffer emacs-telescope--results-buffer
      (let ((inhibit-read-only t)
            (buffer-read-only nil)) ; Ensure buffer is writable
        (erase-buffer)
        ;; Check if results exist before processing
        (if (null emacs-telescope--results)
            (insert "No results available.")
          (dotimes (i (length emacs-telescope--results))
            (let ((item (nth i emacs-telescope--results)))
              (if (= i emacs-telescope--current-selection)
                  ;; Use the face defined in the UI module
                  (insert (propertize (format "> %s\n" item) 'face emacs-telescope-ui-selection-face))
                (insert (format "  %s\n" item))))))
        ;; Make buffer read-only again after modification
        (setq buffer-read-only t)))

    ;; Trigger preview update *after* results buffer is updated
    ;; Check if selection is valid before getting item
    (when (and emacs-telescope--results
               (>= emacs-telescope--current-selection 0)
               (< emacs-telescope--current-selection (length emacs-telescope--results)))
      (let ((selected-item (nth emacs-telescope--current-selection emacs-telescope--results)))
        ;; Call the correct UI function with the selected item
        (emacs-telescope--update-preview)))))


;; Add this function definition back into emacs-telescope.el
(defun emacs-telescope--update-preview ()
  "Update the preview based on current selection."
  (when emacs-telescope--preview-timer
    (cancel-timer emacs-telescope--preview-timer))

  (setq emacs-telescope--preview-timer
        (run-with-timer
         emacs-telescope-preview-delay nil
         ;; Lambda function to perform the preview update
         (lambda ()
           ;; Ensure results exist and selection is valid
           (when (and emacs-telescope--results
                      (>= emacs-telescope--current-selection 0)
                      (< emacs-telescope--current-selection (length emacs-telescope--results)))
             (let ((selected (nth emacs-telescope--current-selection emacs-telescope--results))
                   ;; Get project root once for potential use
                   (project-root (project-root (project-current t))))
               (when (and emacs-telescope--preview-buffer (buffer-live-p emacs-telescope--preview-buffer))
                 (with-current-buffer emacs-telescope--preview-buffer
                   (let ((inhibit-read-only t) ;; Allow modification
                         (buffer-read-only nil)) ;; Ensure buffer isn't read-only
                     (erase-buffer) ;; Clear previous preview
                     (condition-case err ; Basic error handling
                         (cond                                    
                          ;; Case 1: Grep result preview (format: file:line:content)
                          ((string-match "\\(.+?\\):\\([0-9]+\\):\\(.*\\)" selected) ; Use non-greedy match for file
                           (let* ((relative-file (match-string 1 selected))
                                  (file (if project-root
                                            (expand-file-name relative-file project-root)
                                          relative-file))
                                  (line (string-to-number (match-string 2 selected)))
                                  (content (match-string 3 selected)))
                             (if (and (file-exists-p file) (file-readable-p file))
                                 (progn
                                   (insert-file-contents file nil nil nil t)
                                   (let ((mode (or (derived-mode-p 'prog-mode)
                                                   (assoc-default file auto-mode-alist 'string-match))))
                                     (when mode
                                       (with-demoted-errors "Error setting mode: %S" (funcall mode))))
                                   ;; Go to the target line
                                   (goto-char (point-min))
                                   (forward-line (1- line))
                                   ;; *** MODIFIED HIGHLIGHTING START ***
                                   (let ((line-start (line-beginning-position))
                                         (line-end (line-end-position)))
                                     ;; 1. Apply base highlight to the entire line
                                     (put-text-property line-start line-end 'face emacs-telescope-ui-selection-face)

                                     ;; 2. If query exists, try to highlight the specific match on top
                                     (when (and emacs-telescope--current-query
                                                (not (string-empty-p emacs-telescope--current-query))
                                                ;; Search within the current line only
                                                (save-excursion
                                                  (goto-char line-start)
                                                  ;; Use regexp-quote for literal search, ignore case (t)
                                                  (search-forward (regexp-quote emacs-telescope--current-query) line-end t)))
                                       ;; If found, apply 'match' face specifically to the query text
                                       (put-text-property (match-beginning 0) (match-end 0) 'face 'match)))
                                   ;; *** MODIFIED HIGHLIGHTING END ***
                                   (recenter (/ (window-height) 2))) ; Center view
                               ;; Handle file not found/readable
                               (insert (format "File not found or not readable: %s\n\nMatched content:\n%s"
                                               file content)))))


                          ;; Case 2: File preview (could be relative or absolute)
                          ((and (stringp selected)
                                (let ((file-to-check (if project-root
                                                         (expand-file-name selected project-root)
                                                       selected)))
                                  (file-exists-p file-to-check)))
                           (let* ((file-to-check (if project-root
                                                     (expand-file-name selected project-root)
                                                   selected))
                                  (file file-to-check)) ; Use the potentially expanded path
                             (if (file-readable-p file)
                                 (progn
                                   (insert-file-contents file nil nil nil t)
                                   ;; Attempt to set the correct major mode
                                   (let ((mode (or (derived-mode-p 'prog-mode)
                                                   (assoc-default file auto-mode-alist 'string-match))))
                                     (when mode
                                       (with-demoted-errors "Error setting mode: %S"
                                         (funcall mode))))
                                   (goto-char (point-min)) ; Go to start of file
                                   (recenter 0)) ; Show top of file
                               (insert (format "File not readable: %s" file)))))

                          ;; Case 3: Buffer preview
                          ((and (stringp selected) (get-buffer selected))
                           (let ((buffer (get-buffer selected)))
                             (with-current-buffer buffer
                               ;; Insert buffer content into preview
                               (insert-buffer-substring buffer)
                               ;; Try to set the mode based on the original buffer's mode
                               (when major-mode
                                 (with-current-buffer emacs-telescope--preview-buffer
                                   (with-demoted-errors "Error setting mode: %S"
                                     (funcall major-mode))))))
                           (goto-char (point-min))
                           (recenter 0))

                          ;; Default Case: No preview available
                          (t (insert (format "No preview available for: %s" selected))))
                       ;; Catch errors during preview generation
                       (error (insert (format "Error generating preview for %s:\n%s" selected err)))))))))))))


(defun emacs-telescope-select-item ()
  "Select the current item."
  (interactive)
  (when (and emacs-telescope--results
             (>= emacs-telescope--current-selection 0)
             (< emacs-telescope--current-selection (length emacs-telescope--results)))
    (let ((selected (nth emacs-telescope--current-selection emacs-telescope--results))
          ;; Get project root for potential use in grep/file cases
          (project-root (project-root (project-current t))))
      (emacs-telescope-quit) ; Quit UI first
      (cond
       ;; Grep result (file:line:content)
       ((string-match "\(.+?\):\([0-9]+\):.*" selected) ; Use non-greedy regex matching preview
        (let* ((relative-file (match-string 1 selected))
               (line (string-to-number (match-string 2 selected)))
               ;; Construct absolute path relative to project root
               (file (expand-file-name relative-file project-root)))
          (when (file-exists-p file) ; Check existence of absolute path
            (find-file file)
            (goto-char (point-min))
            (forward-line (1- line))
            (recenter)))) ; Optional: recenter after jumping

       ;; File selection (assuming relative path from project root)
       ((and (stringp selected) project-root ; Ensure project-root is valid
             (let ((file (expand-file-name selected project-root))) ; Construct absolute path
               (file-exists-p file))) ; Check existence
        (find-file (expand-file-name selected project-root))) ; Open absolute path

       ;; Buffer selection
       ((and (stringp selected) (get-buffer selected))
        (switch-to-buffer (get-buffer selected)))

       ;; Fallback: If it's a string but not matched above, maybe try opening as file?
       ;; This handles cases where find-files might return absolute paths if not in a project.
       ((and (stringp selected) (file-exists-p selected))
        (find-file selected))

       (t (message "Don't know how to open: %s" selected))
       ))))


(defun emacs-telescope-quit ()
  "Quit telescope."
  (interactive)
  (when emacs-telescope--preview-timer
    (cancel-timer emacs-telescope--preview-timer))
  (when emacs-telescope--filter-timer ; <-- Add this line
    (cancel-timer emacs-telescope--filter-timer)) ; <-- Add this line
  (setq emacs-telescope--current-query nil)

;; Remove the post-command-hook function
  (remove-hook 'post-command-hook #'emacs-telescope--check-input-change-via-post-command)


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
    (setq emacs-telescope--current-query nil) ; Clear grep query
    (setq emacs-telescope--original-results files) ; Store original
    (setq emacs-telescope--results files)         ; Set initial display results
    (setq emacs-telescope--current-selection 0)
    (emacs-telescope--create-ui)
    (emacs-telescope--update-selection)))

;;;###autoload
(defun emacs-telescope-buffers ()
  "Find buffers using telescope."
  (interactive)
  (let* ((buffers (mapcar #'buffer-name (buffer-list))))
    (setq emacs-telescope--current-query nil) ; Clear grep query
    (setq emacs-telescope--original-results buffers) ; Store original
    (setq emacs-telescope--results buffers)         ; Set initial display results
    (setq emacs-telescope--current-selection 0)
    (emacs-telescope--create-ui)
    (emacs-telescope--update-selection)))


(defvar emacs-telescope--last-input-tick -1 ; Initialize to -1
  "Buffer modification tick of the input buffer from the last check.")
(defvar emacs-telescope--last-input-length -1 ; Initialize to -1
  "Buffer length of the input buffer from the last check.")

(defun emacs-telescope--check-input-change-via-post-command ()
  "Check if telescope input changed and trigger filtering. Runs via post-command-hook."
  ;; Only run if the telescope input buffer is live and is the current buffer
  (when (and emacs-telescope--buffer ; Check if UI is active
             (buffer-live-p emacs-telescope--buffer)
             (eq (current-buffer) emacs-telescope--buffer))
    ;; Check if buffer content actually changed since last time
    (let ((current-tick (buffer-modified-tick emacs-telescope--buffer))
          (current-length (buffer-size emacs-telescope--buffer)))
      (unless (and (= current-tick emacs-telescope--last-input-tick)
                   (= current-length emacs-telescope--last-input-length))
        ;; Content changed! Update last known state
        (setq emacs-telescope--last-input-tick current-tick)
        (setq emacs-telescope--last-input-length current-length)

        ;; Now, run the filtering logic (similar to the original hook)
        (message "DEBUG: Input change detected via post-command-hook") ; Debug message
        (let ((query "")
              (prompt-found-p nil))
          (save-excursion
            (goto-char (point-min))
            (when (search-forward emacs-telescope-ui-prompt nil t)
              (setq query (buffer-substring-no-properties (point) (point-max)))
              (setq prompt-found-p t)))
          (when prompt-found-p
            (let ((filtered (emacs-telescope--filter-results query emacs-telescope--original-results)))
              (setq emacs-telescope--results filtered)
              (setq emacs-telescope--current-selection 0)
              (when emacs-telescope--filter-timer (cancel-timer emacs-telescope--filter-timer))
              (setq emacs-telescope--filter-timer
                    (run-with-timer 0.05 nil #'emacs-telescope--update-selection)))))))))



;;;###autoload
;; Grep functionality is now in emacs-telescope-grep.el
(declare-function emacs-telescope-grep "emacs-telescope-grep")

(provide 'emacs-telescope)
;;; emacs-telescope.el ends here
