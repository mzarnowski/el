;;; randomize-workspace.el --- Organize Your Work -*- lexical-binding: t; -*-
(require 'cl-lib)
(require 'org-id)
(defalias 'uuid #'org-id-new)

(require 'workspace)

(cl-defun randomize-workspace-root ()
  (custom-set-variables '(workspace-root (concat "/tmp/" (uuid)))))

(cl-defun random-workspace (&key (name nil))
  (list
   `(workspace-path . ,(concat "/tmp/" (uuid)))))

(provide 'randomize-workspace)
;;; randomize-workspace.el --- ends here
