;;; sysctl.el -- Manage sysctl through emacs  -*- lexical-binding: t -*-
;;; Commentary:
;; This package allows you to view sysctl in a hierarchal structure in Emacs.
;;; Code:

(require 'subr-x)
(require 'org)

(defvar sysctl-buffer-name "*sysctl*"
  "Default name of the sysctl buffer.")

(defun sysctl-run (args)
  "Run `sysctl' with the ARGS arguments, run with root if AS-ROOT is non-nil."
  (shell-command-to-string (concat "sysctl " args)))

(defun sysctl-separator ()
  "System dependant syscl separator."
  (let ((system (shell-command-to-string "uname -s")))
    (cond ((string= system "Darwin\n") ": ")
          ((string= system "OpenBSD\n") "=")
          ((string= system "FreeBSD\n") ": ")
          ((string= system "Linux\n") " = ")
          (t ": "))))

(defun sysctl-split-line (line separator)
  "Split LINE into key and value, splitting with SEPARATOR."
  (let (key value)
    (when (string-match (concat "\\(.*?\\)" separator "\\(.*\\)$") line)
        (save-match-data
          (setq key (split-string (match-string 1 line) "\\.")))
      (setq value (match-string 2 line))
      (cons key value))))

(defun sysctl-split-lines (lines)
  "Split LINES into lines, keys, values."
  (let (output
        (separator (sysctl-separator))
        (split-lines (split-string lines "\n")))
    (dolist (line split-lines output)
      (if-let (split-line (sysctl-split-line line separator))
          (push split-line output)))
    (nreverse output)))

(defun sysctl--readonly-previous-line ()
  "Set the current line to read-only."
  (forward-line -1)
  (let ((begin (line-beginning-position))
        (end (line-end-position)))
    (setq begin (if (eq begin 1) 1 (1- begin)))
    (set-text-properties begin end '(read-only t))
    (forward-line)))

(defun sysctl-construct-tree (lines-list)
  "Turn LINES-LIST into an org hierarchy."
  (let (path)
    (dolist (line lines-list)
      (let ((line-path (car line))
            (line-value (cdr line))
            (depth 1))
        (while line-path
          (unless (string= (nth (- depth 1) path)
                           (car line-path))
            (insert (concat (make-string depth ?*) " " (car line-path) "\n"))
            (sysctl--readonly-previous-line))
          (setq line-path (cdr line-path)
                depth (1+ depth)))
        (setq path (car line))
        (insert (concat line-value "\n"))))))

;;;###autoload
(defun sysctl ()
  "Construct an Org buffer from the sysctl tree."
  (interactive)
  (switch-to-buffer sysctl-buffer-name)
  (erase-buffer)
  (sysctl-construct-tree (sysctl-split-lines (sysctl-run "-a")))
  (sysctl-mode)
  (if (boundp flyspell-mode)
      (flyspell-mode-off)))

(defun sysctl-construct-command ()
  "Construct a sysctl command from the current position in the tree."
  (save-excursion
    (let ((value (string-trim (thing-at-point 'line t)))
          path)
      (if (org-at-heading-p)
          (message "The point must be on a value.")
        (outline-previous-heading)
        (push (substring-no-properties (org-get-heading t t t t)) path)
        (while (org-up-heading-safe)
          (push (substring-no-properties (org-get-heading t t t t)) path))
        (concat (string-join path ".") "=" value)))))

(defun sysctl-superuser-cmd ()
  "Return the system's super user command."
  (let ((system (shell-command-to-string "uname -s")))
    (cond
     ((string= system "OpenBSD\n") "doas")
     (t "sudo"))))

(defun sysctl-construct-tramp ()
  "Construct the TRAMP command required to run a command as root."
  (let ((dir default-directory))
    (if (not (string-prefix-p "/ssh" dir))
        (concat "/" (sysctl-superuser-cmd) "::"))))

(defun sysctl-set-value ()
  "Set the value of the current leaf on the tree in sysctl."
  (interactive)
  (if-let* ((sysctl-cmd (sysctl-construct-command)))
      (if (y-or-n-p (concat "Set " sysctl-cmd "?"))
          (let ((default-directory (sysctl-construct-tramp)))
            (sysctl-run sysctl-cmd))
        (message "Not set."))))

(defvar sysctl-mode-map
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "C-c C-c") 'sysctl-set-value)
        map))

(define-derived-mode sysctl-mode org-mode "Sysctl"
  "Mode for managing sysctl configs.")

(provide 'sysctl)
;;; sysctl.el ends here
