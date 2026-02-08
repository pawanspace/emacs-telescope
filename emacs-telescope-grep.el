(require 'project)
(require 'subr-x) ; For string-split

(defcustom emacs-telescope-grep-file-types
  '("*.el" "*.py" "*.js" "*.java" "*.c" "*.cpp" "*.rs" "*.html" "*.css" "*.md")
  "List of file types to include in grep searches."
  :type '(repeat string)
  :group 'emacs-telescope)

(defun emacs-telescope-grep--build-command (query project-root)
  "Build the grep command string."
  ;; Use shell-quote-argument for safety against special characters in query or filenames
  (let* ((quoted-query (shell-quote-argument query))
         (include-args (mapconcat (lambda (type)
                                    (shell-quote-argument (format "--include=%s" type)))
                                  emacs-telescope-grep-file-types
                                  " ")))
    ;; -n: line number, -H: filename, -r: recursive
    ;; Using . as the path to search within the project-root
    ;; Redirect stderr to /dev/null to hide errors like permission denied
    (format "grep -nHr %s %s . 2>/dev/null"
            quoted-query
            include-args)))

(defun emacs-telescope-grep-get-results (query)
  "Get grep results for QUERY in the current project.
Returns a list of strings, each formatted as 'filename:lineno:match'."
  (let* ((project-root (project-root (project-current t)))
         (default-directory project-root) ; Crucial for grep to search in the right place
         (grep-cmd (emacs-telescope-grep--build-command query project-root))
         (raw-output (shell-command-to-string grep-cmd)))
    ;; Split the output into lines, removing any empty lines
    (string-split raw-output "\n" t)))

;; In emacs-telescope-grep.el
;;;###autoload
(defun emacs-telescope-grep ()
  "Grep in project using telescope with live filtering."
  (interactive)
  ;; Set up for live grep mode
  (setq emacs-telescope--current-source 'grep)
  (setq emacs-telescope--current-query "")
  (setq emacs-telescope--results nil)
  (setq emacs-telescope--original-results nil)
  (setq emacs-telescope--current-selection 0)
  (emacs-telescope--create-ui)
  (emacs-telescope--update-selection))

(provide 'emacs-telescope-grep)
;;; emacs-telescope-grep.el ends here
