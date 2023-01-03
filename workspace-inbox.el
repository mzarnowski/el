;;; workspace-inbox.el --- TODO -*- lexical-binding: t; -*-

(require 'cl-lib)

(require 'workspace-file)

(cl-defstruct workspace-inbox-entry id title status source created-on)
(cl-defstruct range buffer start end)

(cl-defun workspace-inbox-explore (workspace)
  (let ((buffer (workspace-inbox-get-buffer-create workspace)))
    (with-current-buffer buffer (revert-buffer))
    (switch-to-buffer buffer)))

(cl-defun workspace-inbox-get-buffer-create (workspace)
  (let ((name (format "%s:inbox" "TODO")))
    (or (get-buffer name)
	(workspace-inbox-make-buffer workspace name))))

(cl-defun workspace-inbox-make-buffer (workspace buffer-name)
  (let ((buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (workspace-inbox-mode)
      (workspace-inbox-initialize-buffer workspace))
    buffer))

(setq workspace-inbox-mode-map
  (workspace-define-keymap
   `("RET" "open" (lambda () (interactive) (workspace-inbox-open-entry)))))

(cl-defun workspace-inbox-open-entry ()
  "Runs within the "
  (unless (derived-mode-p 'workspace-inbox-mode)
    (error "Not an inbox: " major-mode))

  (let* ((id     (tabulated-list-get-id))
	 (range  (get-text-property (point-min) :source id))
	 (entry-buffer (concat (buffer-name) ":" id)))
    (with-current-buffer (get-buffer-create entry-buffer)
      (unless (derived-mode-p 'org-mode)
	;; buffer was just created, let's initialize it
	(insert-buffer-substring (range-buffer range)
				 (range-start  range)
				 (range-end    range))
	(org-mode)
	(org-hide-drawer-all))
      
      (switch-to-buffer (current-buffer)))))

(define-derived-mode workspace-inbox-mode tabulated-list-mode "Inbox"
  (add-hook 'tabulated-list-revert-hook #'workspace-inbox-buffer-refresh nil t)
  (setq-local tabulated-list-padding  1
	      tabulated-list-sort-key '("Created" . t)))

(cl-defun workspace-inbox-buffer-refresh ()
  (unless current-workspace
      (error "inbox buffer not initialized"))
  (let ((entries (workspace-inbox-entries current-workspace)))
    (setq-local tabulated-list-entries
		(-map #'workspace-inbox-entry-format entries))
    (tabulated-list-print)))

(cl-defun workspace-inbox-entry-format (entry)
  (let ((id      (workspace-inbox-entry-id         entry))
	(source  (workspace-inbox-entry-source     entry))
	(created (workspace-inbox-entry-created-on entry))
	(status  (workspace-inbox-entry-status     entry))
	(title   (workspace-inbox-entry-title      entry)))
    (list (propertize id :source source)
	  (vector created status title))))

(cl-defun workspace-inbox-initialize-buffer (workspace &optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (unless (eq major-mode 'workspace-inbox-mode)
      (error "not in workspace-inbox-mode"))

    (setq-local current-workspace workspace)
    
    (let ((header [("Created" 10 t) ("Status" 6 t) ("Title" 1 t)]))
      (setq-local tabulated-list-format header)
      (tabulated-list-init-header))))

(cl-defun workspace-inbox-entries (workspace)
  (let ((files (f-files (workspace-inbox-directory workspace) nil t))
	(query '(and (property "ID") (todo))))
    (org-ql-select files query :action #'workspace-inbox-entry-parser)))

(cl-defun workspace-inbox-directory (workspace) 
  (workspace-file-resolve-directory workspace "inbox"))

(cl-defun workspace-inbox-entry-parser ()
  (when-let* ((plist (nth 1 (org-element-headline-parser (point-max))))
	      (id (plist-get plist ':ID)))
    (make-workspace-inbox-entry
     :id         id
     :title      (plist-get plist ':raw-value)
     :status     (->> (plist-get plist ':todo-keyword)
		      (substring-no-properties))
     :source     (make-range :buffer (current-buffer)
			     :start (plist-get plist :begin)
			     :end (plist-get plist :end))
     :created-on (->> (plist-get plist ':scheduled)
		      (org-timestamp-to-time)
		      (format-time-string "%Y-%m-%d")))))

(provide 'workspace-inbox)
;;; workspace-inbox.el ends here
