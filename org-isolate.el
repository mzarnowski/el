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

(cl-defstruct org-isolate-origin buffer position id hash)

(cl-defun org-isolate-subtree-at-point ()
  ;; TODO let's require that current buffer is at the correct position
  (let* ((id (or (org-id-get) (error "No id for current subtree")))
	 (buffer (get-buffer-create (org-isolate-buffer-name id)))
	 (original-buffer   (current-buffer))
	 (original-position (point)))
    (when (= 0 (buffer-size buffer))
      ;; insert content
      (let ((start-position (org-isolate--current-subtree-start))
	    (end-position   (org-isolate--current-subtree-end  )))
        (insert-into-buffer buffer start-position end-position))

      (with-current-buffer buffer
	(org-mode)

	(setq-local org-isolate-origin
		    (make-org-isolate-origin :buffer   original-buffer
					     :position original-position
					     :id       id
					     :hash     (sha1 (current-buffer))))
	;; TODO initialize buffer-locals
	;; start at the beginning of the buffer
	(goto-char (point-min))))
    (switch-to-buffer buffer)))

(cl-defun org-isolate-save-current-buffer ()
  (interactive)
  (let* ((isolated-buffer (current-buffer))
	 (origin          (or org-isolate-origin (error "Missing isolation origin")))
	 (origin-id       (org-isolate-origin-id origin))
	 (origin-buffer   (org-isolate-origin-buffer origin))
	 (origin-hash     (org-isolate-origin-hash origin))
	 (origin-position (org-isolate-origin-position origin)))
    (unless (string= origin-hash (sha1 (current-buffer)))
      (with-current-buffer origin-buffer
	(save-excursion
	  ;; go to the origin heading
	  (goto-char origin-position)
	  (org-back-to-heading t)
	  (unless (string= origin-id (org-id-get))
	    (goto-char (org-find-entry-with-id origin-id)))

	  (let ((start-position (org-isolate--current-subtree-start))
		(end-position   (org-isolate--current-subtree-end  )))
	    ;; TODO check, if actual entry has changed. Ask to overwrite?
	    (delete-region start-position end-position)
	    (with-current-buffer isolated-buffer
	      (insert-into-buffer origin-buffer))
	    (unless (= (point) (buffer-end 1))
	      (newline))))
	(save-buffer)))
    (kill-buffer)))

(provide 'sub-org)
