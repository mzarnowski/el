;;; -*- lexical-binding: t; -*-
(require 'buttercup)

(require 'workspace)
(require 'workflow)
(require 'randomize-workspace)

(require 'workspace-capture)

(describe "a capture buffer"
  (it "is not bound to any file"
    (let* ((window (workspace-capture))
	   (buffer (window-buffer window)))
      (expect (buffer-file-name buffer) :to-be nil)))

  (it "is not reused across invocations"
    (let* ((window-1 (workspace-capture))
	   (window-2 (workspace-capture)))
      (expect (window-buffer window-1) :to-be (window-buffer window-2))))
  
  (it "reuses the same window"
    (let* ((window-1 (workspace-capture))
	   (window-2 (workspace-capture)))
      (expect window-1 :to-be window-2))))

(describe "discarding an unmodified capture buffer"
  (it "does not require confirmation"
    (spy-on #'workspace-capture-discard :and-call-through)
    (spy-on #'workspace-select :and-return-value (random-workspace))
    (spy-on #'y-or-n-p :and-return-value t)
    
    (let* ((window (workspace-capture))
	   (buffer (window-buffer window)))
      (with-current-buffer buffer
	(workflow-abort))

      (expect #'workspace-capture-discard :to-have-been-called)
      (expect #'y-or-n-p :not :to-have-been-called)
      (expect (buffer-live-p buffer) :to-be nil))))

(describe "discarding a modified capture buffer"  
  (it "requires confirmation"
    (spy-on #'workspace-capture-discard :and-call-through)
    (spy-on #'workspace-select :and-return-value (random-workspace))
    (spy-on #'y-or-n-p :and-return-value t)
    
    (let* ((window (workspace-capture))
	   (buffer (window-buffer window)))
      (with-current-buffer buffer
	(insert "Title")
        (workflow-abort))

      (expect #'workspace-capture-discard :to-have-been-called)
      (expect #'y-or-n-p :to-have-been-called)
      (expect (buffer-live-p buffer) :to-be nil)))

  (it "can be aborted"
    (spy-on #'workspace-select :and-return-value (random-workspace))
    (spy-on #'y-or-n-p :and-return-value nil)
    
    (let* ((window (workspace-capture))
	   (buffer (window-buffer window)))
      (with-current-buffer buffer
	(insert "Title")
	(workflow-abort))

      (expect #'y-or-n-p :to-have-been-called)
      (expect (buffer-live-p buffer) :to-be t))))

(describe "a captured entry"
  (before-each
    (spy-on #'workspace-store)
    (spy-on #'workspace-select :and-return-value (random-workspace)))
  
  (it "cannot be empty"
    (let* ((window (workspace-capture))
	   (buffer (window-buffer window)))
      (with-current-buffer buffer
	(expect (workflow-continue) :to-be nil)
	(expect #'workspace-store :not :to-have-been-called))))

  (it "is stored"
    (let* ((org-id-track-globally nil)
	   (window (workspace-capture))
	   (buffer (window-buffer window)))
      (with-current-buffer buffer
	(insert "Title")
	(expect (workflow-continue) :to-be t)
	(expect #'workspace-store :to-have-been-called)))))

(describe "finalizing an entry"
  :var (workspace (random-workspace))
     
  (it "inserts an ID"
    (let* ((window (workspace-capture))
	   (buffer (window-buffer window)))
      (with-current-buffer buffer
	(insert "Title")

	(workspace-capture--wrap-current-entry workspace nil)
	(expect (org-id-get) :not :to-be nil))))

  (it "does not overwrite a user-defined ID"
    (let* ((window (workspace-capture))
	   (buffer (window-buffer window)))
      (with-current-buffer buffer
	(insert "Title")
        (org-set-property "ID" "foo")

	(workspace-capture--wrap-current-entry workspace nil)
	(expect (org-id-get) :to-equal "foo"))))
  
  (it "marks it as TODO"
    (let* ((window (workspace-capture))
	   (buffer (window-buffer window)))
      (with-current-buffer buffer
	(insert "Title")

	(workspace-capture--wrap-current-entry workspace nil)
	(expect (org-entry-is-todo-p) :to-equal '("TODO")))))

  (it "does not overwrite a user-defined todo state"
    (let* ((window (workspace-capture))
	   (buffer (window-buffer window)))
      (with-current-buffer buffer
	(insert "DONE Title")

	(workspace-capture--wrap-current-entry workspace nil)
	(expect (org-entry-is-todo-p) :to-equal nil)
	(expect (org-entry-is-done-p) :to-equal '("DONE")))))
  
  (it "schedules it for revision"
    (let* ((window (workspace-capture))
	   (buffer (window-buffer window)))
      (with-current-buffer buffer
	(insert "Title")

	(workspace-capture--wrap-current-entry workspace nil)
	(expect (org-get-scheduled-time nil) :not :to-be nil))))

  (it "does not overwrite a user-defined schedule"
    (let* ((window (workspace-capture))
	   (buffer (window-buffer window)))
      (with-current-buffer buffer
	(insert "Title")
	(newline)
	(insert "SCHEDULED: <2000-01-01>")
	
	(workspace-capture--wrap-current-entry workspace nil)
	(expect (format-time-string "%Y-%m-%d" (org-get-scheduled-time nil))
		:to-equal "2000-01-01")))))
