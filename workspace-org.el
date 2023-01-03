;;; workspace-org.el --- TODO -*- lexical-binding: t; -*-

(require 'org)

(defun org-find-or-create-headline (headline &optional reposition)
  "Find or create a given headline"
  (when (eq major-mode 'org-mode)
    (save-match-data
      (when reposition (goto-char (point-min)))
      (unless (re-search-forward  (concat "^" headline "$") nil 'no-error)
	(end-of-line)
	(newline)
	(insert headline)))))

(defun org-headline (level name)
  (let ((prefix (s-repeat (+ level 1) "*")))
    (s-concat prefix " " name)))

(defun org-find-or-create-olp (olp)
  (when (eq major-mode 'org-mode)
    (let ((headlines (-map-indexed #'org-headline olp)))
      (goto-char (point-min))
      (-each headlines  #'org-find-or-create-headline)
      (org-end-of-subtree))))

(defun org-find-or-create-category-headline (category)
  "Find or create a given category headline"
  (org-find-or-create-headline (org-headline 0 (s-capitalize category)))
  (org-set-category category)
  (org-end-of-subtree))

(defun org-set-category (category)
  (unless (string= category (org-entry-get-with-inheritance "CATEGORY"))
    (org-set-property "CATEGORY" category)))

(defun org-current-year-month-day ()
  (format-time-string "<%Y-%m-%d>" (current-time)))
(provide 'workspace-org)
;;; workspace-org.el ends here
