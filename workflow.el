;;; workflow.el --- Navigate Your Work  -*- lexical-binding: t; -*-
;; TODO use kill-{buffer,emacs}-query-functions to call the abort action

(require 'workspace)

(cl-defun workflow-continue ()
  (interactive)
  (let ((workspace (workspace-select))
	(mode      nil))
    (funcall workflow-continue-action workspace mode)))

(cl-defun workflow-abort ()
  (interactive)
  (let ((workspace (workspace-select))
	(mode      nil))
    (funcall workflow-abort-action workspace mode)))

(provide 'workflow)
;;; workflow.el ends here
