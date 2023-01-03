;;; workspace-capture.el --- Free your mind -*- lexical-binding: t; -*-

(require 'cl-lib)

(require 'workspace)
(require 'workspace-file)
(require 'workspace-org)

(define-derived-mode workspace-capture-mode org-mode "Capture")

(defcustom workspace-capture-display-buffer-alist
  '(display-buffer-reuse-mode-window (inhibit-switch-frame . t))
  "")

(cl-defun workspace-capture ()
  (interactive)
  (let ((buffer (generate-new-buffer "*inbox*")))
    (workspace-capture--initialize-buffer buffer)
    (workspace-capture--show-buffer       buffer)))
  
(cl-defun workspace-capture--initialize-buffer (buffer)
  (with-current-buffer buffer
    (workspace-capture-mode)
    (insert "* ")

    (setq-local workflow-continue-action #'workspace-capture-commit
		workflow-abort-action    #'workspace-capture-discard)))

(cl-defun workspace-capture--show-buffer (buffer)
  (let ((display-alist workspace-capture-display-buffer-alist))
    (select-window (display-buffer buffer display-alist))))

(cl-defun workspace-capture-empty-buffer-p ()
  (string= "* " (buffer-string)))

(cl-defun workspace-capture-discard (workspace mode)
  (when (or (workspace-capture-empty-buffer-p)
	    (y-or-n-p "Discard entry?")) ;; TODO we could include the title here
    (kill-buffer)))

(defun workspace-capture-commit (workspace mode)
  (unless (workspace-capture-empty-buffer-p)
    (workspace-capture--wrap-current-entry  workspace mode)
    (workspace-capture--store-current-entry workspace mode)
    (kill-buffer)))

(defun workspace-capture--wrap-current-entry (workspace mode)
  (goto-char (point-min))

  ;; make the entry a TODO
  (unless (org-get-todo-state)
    (org-todo "TODO"))

  ;; insert an ID
  (unless (org-id-get)
    (org-set-property "ID" (org-id-new)))
  
  ;; schedule refinement, unless already scheduled by the author
  (unless (org-entry-get nil "SCHEDULED")
    (let ((org-time-stamp-formats '("<%Y-%m-%d>")))
      (org-entry-put nil "SCHEDULED" (org-current-year-month-day)))))

(defun workspace-capture--store-current-entry (workspace mode)
  (workspace-store workspace
		   (current-buffer)
		   (make-workspace-file
			   :directory "inbox"
			   :extension "org"
			   :name (format-time-string "%Y"))
		   #'point-max))

(provide 'workspace-capture)
;;; workspace-capture.el ends here
