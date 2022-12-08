;;; Edit org subtrees in isolation before commiting the changes to the original file.

(require 'cl-lib)

(define-derived-mode org-isolate-mode org-mode "sub-org")

(cl-defstruct org-isolated-subtree owner id content)

(cl-defun org-isolate-at-point ()
  (org-isolate (current-buffer) (point)))

(cl-defun org-isolate-marked-subtree (marker)
  (org-isolate (marker-buffer marker) (marker-position position)))

(cl-defun org-isolate (buffer position)
  (with-current-buffer buffer
    (save-excursion
      (goto-char position)
      (org-isolate-subtree-at-point))))

(cl-defun org-isolate--current-subtree-start ()
  (save-excursion
    (org-back-to-heading-or-point-min t)
    (point)))

(cl-defun org-isolate--current-subtree-end ()
  (save-excursion
    (org-end-of-subtree t t)
    (point)))

(cl-defun org-isolate-buffer-name (id)
  (format "*isolated-%s*" id))

(cl-defun org-isolate-subtree-at-point ()
  ;; TODO let's require that current buffer is at the correct position
  (let* ((id (or (org-id-get) (error "No id for current subtree")))
	 (buffer (get-buffer-create (org-isolate-buffer-name id))))
    (when (= 0 (buffer-size buffer))
      ;; insert content
      (let ((start-position (org-isolate--current-subtree-start))
	    (end-position   (org-isolate--current-subtree-end  )))
        (insert-into-buffer buffer start-position end-position))

      (with-current-buffer buffer
	(org-mode)
	;; TODO initialize buffer-locals
	;; start at the beginning of the buffer
	(goto-char (point-min))))
    (switch-to-buffer buffer)))

(provide 'sub-org)
