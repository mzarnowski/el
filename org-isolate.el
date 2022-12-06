;;; Edit org subtrees in isolation before commiting the changes to the original file.

(require 'cl-lib)

(define-derived-mode org-isolate-mode org-mode "sub-org")

(cl-defstruct org-isolated-subtree owner id content)

(cl-defun org-isolate-from (marker)
  (with-current-buffer (marker-buffer marker)
    (save-excursion
      (goto-char (marker-position marker))
      (make-org-isolated-subtree :owner   (buffer-file-name buffer)
				 :id      (org-id-get)
				 :content (org-copy-subtree)))))

(cl-defun org-isolate-isolate (marker &optional buffer)
  (let* ((buffer (or buffer (current-buffer)))
	 (sub-tree (org-isolate-from marker)))
    ))

(cl-defun org-isolate-make-buffer (marker name))

(provide 'sub-org)
