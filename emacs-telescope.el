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

;; Declare functions from emacs-telescope-grep
(declare-function emacs-telescope-grep-get-results "emacs-telescope-grep")

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

(defvar emacs-telescope--preview-window nil
  "Window used for telescope preview.")

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

;; Preview helpers
(defun emacs-telescope--apply-preview-mode (&optional file mode)
  "Apply a major MODE (or infer from FILE) and enable font-lock."
  (when mode
    (with-demoted-errors "Error setting mode: %S"
      (funcall mode)))
  (when file
    (setq-local buffer-file-name file)
    (setq-local buffer-file-truename (file-truename file))
    (if (fboundp 'delay-mode-hooks)
        (delay-mode-hooks
          (with-demoted-errors "Error setting auto mode: %S"
            (set-auto-mode)))
      (with-demoted-errors "Error setting auto mode: %S"
        (set-auto-mode))))
  (when (fboundp 'font-lock-ensure)
    (font-lock-ensure)))


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
         (input-buffer-name "*telescope-input*")
         (results-buffer-name "*telescope-results*")
         (preview-buffer-name "*telescope-preview*")
         ;; Kill old buffers if they exist
         (input-buffer (progn
                         (when (get-buffer input-buffer-name)
                           (kill-buffer input-buffer-name))
                         (get-buffer-create input-buffer-name)))
         (results-buffer (progn
                           (when (get-buffer results-buffer-name)
                             (kill-buffer results-buffer-name))
                           (get-buffer-create results-buffer-name)))
         (preview-buffer (progn
                           (when (get-buffer preview-buffer-name)
                             (kill-buffer preview-buffer-name))
                           (get-buffer-create preview-buffer-name)))
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
      
      ;; Store preview window reference globally
      (setq emacs-telescope--preview-window preview-window)

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
          (let ((map (make-sparse-keymap)))
            (set-keymap-parent map minibuffer-local-map)
            (define-key map (kbd "<down>") 'emacs-telescope-next-item)
            (define-key map (kbd "<up>") 'emacs-telescope-prev-item)
            (define-key map (kbd "RET") 'emacs-telescope-select-item)
            (define-key map (kbd "C-g") 'emacs-telescope-quit)
            (use-local-map map))
        ))

      ;; --- Setup Results Buffer ---
      (select-window (next-window results-window))
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
            (let* ((item (nth i emacs-telescope--results))
                   (start-pos (point)))
              (if (= i emacs-telescope--current-selection)
                  ;; Use the face defined in the UI module
                  (insert (propertize (format "> %s\n" item) 'face emacs-telescope-ui-selection-face))
                (insert (format "  %s\n" item)))
              ;; Highlight search query in the result line
              (when (and emacs-telescope--current-query
                         (not (string-empty-p emacs-telescope--current-query)))
                (save-excursion
                  (goto-char start-pos)
                  (let ((line-end (line-end-position)))
                    (while (search-forward emacs-telescope--current-query line-end t)
                      (put-text-property (match-beginning 0) (match-end 0) 'face 'match))))))))
        ;; Make buffer read-only again after modification
        (setq buffer-read-only t))
      
      ;; Position point at the selected line and ensure it's visible
      (goto-char (point-min))
      (forward-line emacs-telescope--current-selection)
      (let ((results-window (get-buffer-window emacs-telescope--results-buffer)))
        (when (window-live-p results-window)
          (set-window-point results-window (point)))))

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
                                   (emacs-telescope--apply-preview-mode file)
                                   ;; Go to the target line
                                   (goto-char (point-min))
                                   (forward-line (1- line))
                                   ;; *** MODIFIED HIGHLIGHTING START ***
                                   (let ((line-start (line-beginning-position))
                                         (line-end (line-end-position))
                                         (target-point (point)))
                                     ;; 1. Apply base highlight to the entire line
                                     (put-text-property line-start line-end 'face emacs-telescope-ui-selection-face)

                                     ;; 2. If query exists, try to highlight the specific match on top
                                     (when (and emacs-telescope--current-query
                                                (not (string-empty-p emacs-telescope--current-query)))
                                       ;; Search within the current line only (case-insensitive)
                                       (let ((case-fold-search t))
                                         (save-excursion
                                           (goto-char line-start)
                                           (while (search-forward emacs-telescope--current-query line-end t)
                                             ;; Apply 'match' face to each occurrence
                                             (put-text-property (match-beginning 0) (match-end 0) 'face 'match)))))
                                     ;; *** MODIFIED HIGHLIGHTING END ***
                                     (when (window-live-p emacs-telescope--preview-window)
                                       (with-selected-window emacs-telescope--preview-window
                                         (with-current-buffer emacs-telescope--preview-buffer
                                           (goto-char target-point)
                                           (recenter (/ (window-height) 2))))))) ; Center view
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
                                   (emacs-telescope--apply-preview-mode file)
                                   (goto-char (point-min)) ; Go to start of file
                                   (when (window-live-p emacs-telescope--preview-window)
                                     (with-selected-window emacs-telescope--preview-window
                                       (with-current-buffer emacs-telescope--preview-buffer
                                         (goto-char (point-min))
                                         (recenter 0))))) ; Show top of file
                               (insert (format "File not readable: %s" file)))))

                          ;; Case 3: Buffer preview
                          ((and (stringp selected) (get-buffer selected))
                           (let ((buffer (get-buffer selected)))
                             ;; Insert buffer content into preview
                             (insert-buffer-substring buffer)
                             ;; Try to set the mode based on the original buffer's mode
                             (let ((mode (buffer-local-value 'major-mode buffer)))
                               (emacs-telescope--apply-preview-mode nil mode))))
                           (goto-char (point-min))
                           (when (window-live-p emacs-telescope--preview-window)
                             (with-selected-window emacs-telescope--preview-window
                               (with-current-buffer emacs-telescope--preview-buffer
                                 (goto-char (point-min))
                                 (recenter 0)))))

                          ;; Default Case: No preview available
                          (t (insert (format "No preview available for: %s" selected))))
                       ;; Catch errors during preview generation
                       (error (insert (format "Error generating preview for %s:\n%s" selected err))))))))))))

(defun emacs-telescope-select-item ()
  "Select the current item."
  (interactive)
  (when (and emacs-telescope--results
             (>= emacs-telescope--current-selection 0)
             (< emacs-telescope--current-selection (length emacs-telescope--results)))
    (let ((selected (nth emacs-telescope--current-selection emacs-telescope--results))
          (project-root (project-root (project-current t))))
      ;; (message "DEBUG select-item: Selected string is: %S" selected) ; Keep commented unless needed
      ;; (message "DEBUG select-item: Project root is: %S" project-root) ; Keep commented unless needed
      (emacs-telescope-quit) ; Quit UI first
      (cond
       ;; Grep result (file:line:content)
       ;; Test with simple regex first
       ((string-match ":[0-9]+:" selected)
        ;; (message "DEBUG select-item: Matched SIMPLE Grep clause.") ; Keep commented unless needed
        ;; Try parsing using split-string instead of complex regex match
        (let* ((parts (split-string selected ":" t)) ; Split by colon, omit empty strings
               (relative-file (when (>= (length parts) 1) (nth 0 parts)))
               (line-str (when (>= (length parts) 2) (nth 1 parts)))
               ;; Combine the rest back in case content had colons
               ;; (content (when (>= (length parts) 3) (mapconcat #'identity (nthcdr 2 parts) ":"))) ; Content not needed for opening
               (line (when (and line-str (string-match-p "^[0-9]+$" line-str)) ; Validate line is numeric
                       (string-to-number line-str)))
               (file (when relative-file ; Only proceed if we got a filename part
                       (if project-root
                           (expand-file-name relative-file project-root)
                         (expand-file-name relative-file default-directory)))))
          ;; Check if parsing was successful (got file and numeric line)
          (if (and file line)
              (progn
                ;; (message "DEBUG select-item: Parsed via split: file=%s, line=%s" file line) ; Keep commented unless needed
                (if (file-exists-p file)
                    (progn
                      (find-file file)
                      (goto-char (point-min))
                      (forward-line (1- line))
                      (recenter))
                  (message "Error: File not found after parsing grep result: %s" file)))
            ;; If parsing failed (e.g., line wasn't numeric), fall through
            ;; (message "DEBUG select-item: Failed to parse grep result via split. Falling through.") ; Keep commented unless needed
            ;; Explicitly signal failure to prevent this clause from fully succeeding
            nil))) ; This makes the cond move to the next clause if parsing fails

       ;; File selection (assuming relative path from project root)
       ((and (stringp selected) project-root
             (let ((file (expand-file-name selected project-root)))
               (file-exists-p file)))
        (find-file (expand-file-name selected project-root)))

       ;; Buffer selection
       ((and (stringp selected) (get-buffer selected))
        (switch-to-buffer (get-buffer selected)))

       ;; Fallback: If it's a string but not matched above, maybe try opening as file?
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

        ;; Extract query from input buffer
        (let ((query "")
              (prompt-found-p nil))
          (save-excursion
            (goto-char (point-min))
            (when (search-forward emacs-telescope-ui-prompt nil t)
              (setq query (buffer-substring-no-properties (point) (point-max)))
              (setq prompt-found-p t)))
          (when prompt-found-p
            ;; Store current query
            (setq emacs-telescope--current-query query)
            
            ;; Handle based on source type
            (cond
             ;; Live grep mode - run grep on each input change
             ((eq emacs-telescope--current-source 'grep)
              (when emacs-telescope--filter-timer 
                (cancel-timer emacs-telescope--filter-timer))
              (setq emacs-telescope--filter-timer
                    (run-with-timer 0.3 nil 
                                    (lambda ()
                                      (if (string-empty-p query)
                                          (progn
                                            (setq emacs-telescope--results nil)
                                            (setq emacs-telescope--original-results nil)
                                            (setq emacs-telescope--current-selection 0)
                                            (emacs-telescope--update-selection))
                                        (let ((results (emacs-telescope-grep-get-results query)))
                                          (setq emacs-telescope--original-results results)
                                          (setq emacs-telescope--results results)
                                          (setq emacs-telescope--current-selection 0)
                                          (emacs-telescope--update-selection)))))))
             
             ;; Normal file/buffer mode - filter from original results
             (t
              (let ((filtered (emacs-telescope--filter-results query emacs-telescope--original-results)))
                (setq emacs-telescope--results filtered)
                (setq emacs-telescope--current-selection 0)
                (when emacs-telescope--filter-timer 
                  (cancel-timer emacs-telescope--filter-timer))
                (setq emacs-telescope--filter-timer
                      (run-with-timer 0.05 nil #'emacs-telescope--update-selection)))))))))))


;;;###autoload
;; Grep functionality is now in emacs-telescope-grep.el
(declare-function emacs-telescope-grep "emacs-telescope-grep")

(provide 'emacs-telescope)
;;; emacs-telescope.el ends here
