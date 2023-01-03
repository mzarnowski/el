;;; workspace-keymap.el --- Workspace Key Maps -*- lexical-binding: t; -*-

(defun workspace-define-keymap (&rest bindings)
  (let ((keymap (make-sparse-keymap)))
    (-each bindings (lambda (binding) (workspace-define-key keymap binding)))
    keymap))

(defun workspace-extend-keymap (parent &rest bindings)
  (when (keymapp parent)
    (let ((keymap (apply #'workspace-define-keymap bindings)))
      (set-keymap-parent keymap parent)
      keymap)))

(defun workspace-define-key (keymap binding)
  (let ((key (nth 0 binding))
	(name (nth 1 binding))
	(command (workspace-normalize-keymap-command (nth 2 binding))))
    (define-key keymap (kbd key) (cons name command))))

(defun workspace-normalize-keymap-command (action)
  (cond ((commandp  action) action)
	((keymapp   action) action)
	((functionp action) `(lambda () (interactive) (,action)))
	((listp     action) `(lambda () (interactive) ,action))
	(_ (user-error "Unsupported command of type %s: %s" (type-of action) action))))

(defun workspace-header--merge-key (prefix key)
  )

(defun workspace-flatmap-keymap (acc keymap f &optional prefix)
  (cl-labels ((merge-key (prefix key)		       
			 (s-trim-left (concat prefix " " suffix)))
	      (map-key (key action)
		       (let ((key (merge-key prefix (single-key-description key))))
			 (cond
			  ((keymapp action) (workspace-flatmap-keymap acc action f key))
			  (t (add-to-list acc (funcall f key action)))))))
    (map-keymap #'map-key keymap))
  acc)

(defun workspace-header--format-key (key)
  ;; leading 'M-' prefix is represented as 'ESC'
  (let ((key (s-replace-regexp "^ESC " "M-" key)))
    (propertize key
		'face           'help-key-binding
		'font-lock-face 'help-key-binding)))

(defun workspace-header--format-command (key name)
  (let ((key (workspace-header--format-key key)))
    (format "%s: '%s'" name key)))

(defun workspace-header--format-binding (key action)
  (pcase action			 
    (`(,name . ,_) (workspace-header--format-command key name))))

(defun workspace-describe-keymap (keymap)
  (let ((properties ()))
    (workspace-flatmap-keymap 'properties keymap #'workspace-header--format-binding)
    (s-join " " properties)))


(provide 'workspace-keymap)
;;; workspace-keymap.el ends here
