;;; workflow.el --- Steer Your Work  -*- lexical-binding: t; -*-
;; TODO use kill-{buffer,emacs}-query-functions to call the abort action

(require 'workspace)

(cl-defun workflow-continue ()
  (interactive)
  (funcall workflow-abort-action (workspace-select)))

(cl-defun workflow-abort ()
  (interactive)
  (funcall workflow-abort-action (workspace-select)))

(provide 'workflow)
;;; workflow.el ends here
