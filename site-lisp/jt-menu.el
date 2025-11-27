;;; jt-menu.el --- Personal command menu using transient -*- lexical-binding: t; -*-

;;; Commentary:
;; A transient-based menu generated from annotations in org config files.
;;
;; Annotation format (in elisp code blocks):
;;   ;; @jt-menu Group | key | Description
;;   (defun my-command () ...)
;;
;; The command is auto-detected from the next defun/use-package form,
;; or can be specified explicitly:
;;   ;; @jt-menu Group | key | Description | explicit-command

;;; Code:

(require 'transient)

(defvar jt/menu-source-files
  (list (expand-file-name "~/.emacs.d/emacs.org")
        (expand-file-name "~/.emacs.d/languages.org"))
  "List of org files to scan for menu annotations.")

(defun jt/menu--parse-annotation (line)
  "Parse a @jt-menu annotation LINE.
Returns (group key description command) or nil."
  (when (string-match ";; @jt-menu \\(.+\\)" line)
    (let* ((content (match-string 1 line))
           (parts (mapcar #'string-trim (split-string content "|"))))
      (when (>= (length parts) 3)
        (list (nth 0 parts)
              (nth 1 parts)
              (nth 2 parts)
              (when (nth 3 parts) (intern (nth 3 parts))))))))

(defun jt/menu--detect-command (buffer pos)
  "Detect the command defined after POS in BUFFER.
Looks for defun, defmacro, or use-package forms."
  (with-current-buffer buffer
    (save-excursion
      (goto-char pos)
      (when (re-search-forward
             "(\\(?:defun\\|defmacro\\|cl-defun\\)\\s-+\\(\\S-+\\)"
             (+ pos 500) t)
        (intern (match-string 1))))))

(defun jt/menu--scan-file (file)
  "Scan FILE for @jt-menu annotations.
Returns list of (group key description command file line)."
  (let ((entries '()))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (let ((line-num 0))
        (while (not (eobp))
          (setq line-num (1+ line-num))
          (let* ((line (buffer-substring-no-properties
                        (line-beginning-position) (line-end-position)))
                 (parsed (jt/menu--parse-annotation line)))
            (when parsed
              (let* ((group (nth 0 parsed))
                     (key (nth 1 parsed))
                     (desc (nth 2 parsed))
                     (explicit-cmd (nth 3 parsed))
                     (detected-cmd (unless explicit-cmd
                                     (jt/menu--detect-command
                                      (current-buffer)
                                      (line-end-position))))
                     (cmd (or explicit-cmd detected-cmd)))
                (if cmd
                    (push (list group key desc cmd file line-num) entries)
                  (warn "jt-menu: No command found for annotation at %s:%d"
                        file line-num)))))
          (forward-line 1))))
    (nreverse entries)))

(defun jt/menu--check-conflicts (entries)
  "Check ENTRIES for conflicting keys or duplicate commands.
Signals an error if conflicts are found."
  (let ((keys (make-hash-table :test 'equal))
        (cmds (make-hash-table :test 'eq))
        (errors '()))
    (dolist (entry entries)
      (let ((key (nth 1 entry))
            (cmd (nth 3 entry))
            (file (nth 4 entry))
            (line (nth 5 entry))
            (location (format "%s:%d" (file-name-nondirectory (nth 4 entry))
                              (nth 5 entry))))
        ;; Check for duplicate keys
        (if-let ((existing (gethash key keys)))
            (push (format "Duplicate key '%s': %s and %s" key existing location)
                  errors)
          (puthash key location keys))
        ;; Check for duplicate commands
        (if-let ((existing (gethash cmd cmds)))
            (push (format "Duplicate command '%s': %s and %s" cmd existing location)
                  errors)
          (puthash cmd location cmds))))
    (when errors
      (error "jt-menu conflicts:\n  %s" (string-join (nreverse errors) "\n  ")))))

(defun jt/menu--group-entries (entries)
  "Group ENTRIES by their group name.
Returns alist of (group . ((key desc cmd) ...))."
  (let ((groups (make-hash-table :test 'equal)))
    (dolist (entry entries)
      (let ((group (nth 0 entry))
            (key (nth 1 entry))
            (desc (nth 2 entry))
            (cmd (nth 3 entry)))
        (puthash group
                 (append (gethash group groups '())
                         (list (list key desc cmd)))
                 groups)))
    (let ((result '()))
      (maphash (lambda (k v) (push (cons k v) result)) groups)
      (nreverse result))))

(defun jt/menu--build-transient-groups (grouped)
  "Build transient group vectors from GROUPED alist."
  (let ((groups '()))
    (dolist (group grouped)
      (let* ((name (car group))
             (items (cdr group))
             (suffixes (mapcar (lambda (item)
                                 (list (nth 0 item)
                                       (nth 1 item)
                                       (nth 2 item)))
                               items)))
        (push (vconcat (list name) suffixes) groups)))
    (nreverse groups)))

(defun jt/menu-generate ()
  "Generate the menu from source files.
Returns the transient group structure."
  (let ((all-entries '()))
    (dolist (file jt/menu-source-files)
      (when (file-exists-p file)
        (setq all-entries (append all-entries (jt/menu--scan-file file)))))
    (jt/menu--check-conflicts all-entries)
    (let* ((grouped (jt/menu--group-entries all-entries))
           (transient-groups (jt/menu--build-transient-groups grouped)))
      ;; Pair up groups into rows of 2
      (let ((rows '())
            (pair '()))
        (dolist (g transient-groups)
          (push g pair)
          (when (= (length pair) 2)
            (push (vconcat (nreverse pair)) rows)
            (setq pair '())))
        (when pair
          (push (vconcat (nreverse pair)) rows))
        (nreverse rows)))))

(defun jt/menu-rebuild ()
  "Rebuild the jt/menu transient from source files."
  (interactive)
  (let ((groups (jt/menu-generate)))
    (eval
     `(transient-define-prefix jt/menu ()
        "Personal command menu."
        ,@groups))
    (message "jt/menu rebuilt with %d groups"
             (length groups))))

;; Build menu on load
(jt/menu-rebuild)

(global-set-key (kbd "C-c m") #'jt/menu)

(provide 'jt-menu)
;;; jt-menu.el ends here
