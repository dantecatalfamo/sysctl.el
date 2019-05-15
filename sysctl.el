;;; sysctl.el -- Manage sysctl through emacs  -*- lexical-binding: t -*-
;;; Commentary:
;; This package allows you to view sysctl in a hierarchal structure in Emacs.
;;; Code:

(require 'subr-x)
(require 'org)

(defvar sysctl-buffer-name "*sysctl*"
  "Default name of the sysctl buffer.")

(defun sysctl-run (args)
  "Run `sysctl' with the ARGS arguments."
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
          (push split-line output)))))


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
            (insert (concat (make-string depth ?*)
                            " "
                            (car line-path)
                            "\n")))
          (setq line-path (cdr line-path)
                depth (1+ depth)))
        (setq path (car line))
        (insert (concat line-value "\n"))))))

(defun sysctl ()
  "Construct an Org buffer from the sysctl tree."
  (interactive)
  (switch-to-buffer sysctl-buffer-name)
  (erase-buffer)
  (sysctl-construct-tree (sysctl-split-lines (sysctl-run "-a")))
  (org-mode)
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
        (concat (string-join path ".") (sysctl-separator) value)))))


(provide 'sysctl)
;;; sysctl.el ends here
