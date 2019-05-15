;;; sysctl.el -- Manage sysctl through emacs  -*- lexical-binding: t -*-
;;; Commentary:
;; This package allows you to view sysctl in a hierarchal structure in Emacs.
;;; Code:

(require 'subr-x)

(defvar sysctl-buffer-name "*sysctl*"
  "Default name of the sysctl buffer.")

(defun sysctl-run (args)
  "Run `sysctl' with the ARGS arguments."
  (shell-command-to-string (concat "sysctl " args)))

(defun sysctl-split-line (line)
  "Split LINE into key and value."
  (let (key value)
    (when (string-match "\\(.*?\\): \\(.*\\)$" line)
        (save-match-data
          (setq key (split-string (match-string 1 line) "\\.")))
      (setq value (match-string 2 line))
      (cons key value))))

(defun sysctl-split-lines (lines)
  "Split LINES into lines, keys, values."
  (let (output
        (split-lines (split-string lines "\n")))
    (dolist (line split-lines output)
      (if-let (split-line (sysctl-split-line line))
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

(defun sysctl-to-org ()
  "Construct an Org buffer from the sysctl tree."
  (interactive)
  (switch-to-buffer sysctl-buffer-name)
  (erase-buffer)
  (sysctl-construct-tree (sysctl-split-lines (sysctl-run "-a")))
  (org-mode))


(provide 'sysctl)
;;; sysctl.el ends here
