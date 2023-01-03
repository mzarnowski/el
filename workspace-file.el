;;; workspace-file.el --- storage - in files -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'f)

(require 'workspace)
(require 'workspace-buffer)

(cl-defun workspace-file-find (workspace &key directory (filter nil) (recursive t))
  (let ((directory (workspace-file-resolve-directory workspace directory)))
    (f-files directory filter recursive)))

(cl-defstruct workspace-file name extension directory)

(cl-defmethod workspace-load (workspace (file workspace-file))
  (find-file-noselect (workspace-file-path workspace file)))

(cl-defmethod workspace-store (workspace source (file workspace-file) position)
  (let ((directory (workspace-file-resolve-directory workspace (workspace-file-directory file))))
    (unless (f-exists? directory)
      (f-mkdir-full-path directory)))
  
  (let ((target-buffer (workspace-load workspace file)))
    (workspace-store workspace source target-buffer position)))

(cl-defun workspace-file-path (workspace file)
  (let ((directory (workspace-file-resolve-directory workspace (workspace-file-directory file)))
	(extension (workspace-file-resolve-extension workspace (workspace-file-extension file))))
    (f-join directory (concat (workspace-file-name file) "." extension))))

(cl-defun workspace-file-resolve-directory (workspace directory)
  (let ((root (alist-get 'workspace-path workspace)))
    (apply #'f-join (cons root (-list directory)))))

(cl-defun workspace-file-resolve-extension (workspace extension)
  ;; TODO handle gpg?
  extension)

(provide 'workspace-file)
;;; workspace-storage.el ends here
