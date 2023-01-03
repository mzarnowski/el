;;; workspace.el --- Organize Your Work -*- lexical-binding: t; -*-
;; TODO how to make killing buffers call the abort function?

(require 'cl-lib)
(require 'f)


(cl-defstruct workspace name path config modes)

(cl-defun workspace-get (key workspace &key (or-else nil))
  (let ((config (workspace-config workspace)))
    (alist-get key config or-else)))

(cl-defun workspace-current-path (workspace directory &key extension)
  (let ((directory (f-join (workspace-path workspace) directory))
	(file-name (format-time-string "%Y" (current-time))))
    (f-join directory (format "%s.%s" file-name extension))))

(cl-defun workspace-compare (f1 f2)
  (and f1 f2 (equal (workspace-name f1) (workspace-name f2))))

(defcustom workspace-provider (lambda () '(((workspace-path . "/tmp/e-workspace")))) "TODO")
(cl-defun workspace-list ()
  "Lists the workspace roots"
  (funcall workspace-provider))

(cl-defun workspace-select ()
  (let ((workspaces (workspace-list)))
    (cond ((null workspaces) (error "No workspace"))
	  ((null (cdr workspaces)) (car workspaces))
	  (t (error "Too many roots")))))

;; storage-related functions
(cl-defgeneric workspace-load (workspace object)
  "Loads the <object> into a buffer")
(cl-defgeneric workspace-store (workspace source target position))

;; TODO
;;  workspace-roots -- map -> ((root . ((workspace-a) (workspace-b))))
;;  where workspace is: ((:name <name>) (:path <full-path>) (:config <a-list>) (:modes <a-list>))
;;  where mode a-list is ((<mode-a> . <property-a-list>)*)

(provide 'workspace)
;;; workspace.el ends here

