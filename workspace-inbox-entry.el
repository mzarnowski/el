;;; workspace-inbox-entry.el --- Steer Your Work  -*- lexical-binding: t; -*-
;; TODO use kill-{buffer,emacs}-query-functions to call the abort action

(require 'workspace)

(cl-defstruct range buffer start end)
(cl-defstruct workspace-inbox-entry id title status source created-on)

(define-derived-mode workspace-inbox-entry-mode org-mode "Inbox Entry")

(cl-defun workspace-inbox-entry-buffer (workspace entry)
  (let ((buffer (get-buffer-create (workspace-inbox-entry-buffer-name workspace entry))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'workspace-inbox-entry-mode)
	(workspace-inbox-entry-mode)
	(workspace-inbox-entry-initialize-buffer workspace entry)))
    buffer))

(cl-defun workspace-inbox-entry-buffer-name (workspace entry)
  (concat "inbox:" (workspace-inbox-entry-id entry)))

(cl-defun workspace-inbox-entry-initialize-buffer (workspace entry)
  (let ((source (workspace-inbox-entry-source entry)))
    (insert-buffer-substring (range-buffer source)
			     (range-start  source)
			     (range-end    source)))
  (org-hide-drawer-all))

(provide 'workspace-inbox-entry)
;;; workflow.el ends here
