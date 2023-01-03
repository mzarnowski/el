;;; -*- lexical-binding: t; -*-
(require 'buttercup)

(require 'workspace)
(require 'workspace-file)

(describe "file storage"
  (it "loads a file"
    (let ((loaded-buffer (workspace-load '((workspace-path . "/tmp/foo"))
					 (make-workspace-file :directory "bar" :name "baz" :extension "xyz"))))
      (with-current-buffer loaded-buffer
	(expect (buffer-file-name) :to-equal "/tmp/foo/bar/baz.xyz"))))

  (it "loads a file from directory list"
    (let ((loaded-buffer (workspace-load '((workspace-path . "/tmp/foo"))
					 (make-workspace-file :directory '("bar-1" "bar-2") :name "baz" :extension "xyz"))))
      (with-current-buffer loaded-buffer
	(expect (buffer-file-name) :to-equal "/tmp/foo/bar-1/bar-2/baz.xyz"))))

  (it "does not create the loaded file, nor its parents"
    (let* ((loaded-buffer (workspace-load '((workspace-path . "/tmp/foo"))
					  (make-workspace-file :directory "bar" :name "baz" :extension "xyz")))
	   (file-name (buffer-file-name loaded-buffer)))
      (expect (f-exists? file-name) :to-be nil)
      (expect (f-exists? (f-parent file-name)) :to-be nil)))

  (it "creates the stored file and its parents"
    (let ((buffer (get-buffer-create "a-buffer"))
	  (workspace `((workspace-path . ,(concat "/tmp/" (number-to-string (random))))))
	  (file (make-workspace-file :directory "bar" :name "baz" :extension "xyz")))
      (with-current-buffer buffer (insert "foo"))

      (workspace-store workspace buffer file #'point-max)

      (let ((file-name (buffer-file-name (workspace-load workspace file))))
	(expect (f-exists? file-name) :to-be t)
	(expect (f-exists? (f-parent file-name)) :to-be t)))))
