;;; workspace-buffer.el ---  storage - in buffers -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'workspace)

(cl-defmethod  workspace-load (workspace (buffer buffer))
  buffer)
(cl-defmethod  workspace-store (workspace source (target buffer) position)
  (with-current-buffer target
    (save-excursion
      (goto-char (funcall position))
      (insert-buffer-substring (workspace-load workspace source))))
  (workspace-save-buffer workspace target))

(cl-defun workspace-save-buffer (workspace buffer)
  (with-current-buffer buffer (save-buffer)))

(provide 'workspace-buffer)
;;; workspace-buffer.el ends here
