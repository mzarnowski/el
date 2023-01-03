;;; -*- lexical-binding: t; -*-
(require 'buttercup)

(require 'workspace)
(require 'workspace-buffer)

(describe "buffer storage"
  (it "loads a buffer"
    (with-temp-buffer
      (let ((loaded-buffer (workspace-load nil (current-buffer))))
	(expect loaded-buffer :to-be (current-buffer)))))

  (it "stores a buffer in another buffer"
    (spy-on #'workspace-save-buffer)
    
    (let ((buffer-A (generate-new-buffer "foo"))
	  (buffer-B (generate-new-buffer "bar")))
      (with-current-buffer buffer-A
	(insert "foo"))
      (with-current-buffer buffer-B
	(insert "-bar-"))

      (workspace-store nil buffer-A buffer-B #'point-min)
      (with-current-buffer buffer-B
	(expect (buffer-string) :to-equal "foo-bar-"))

      (workspace-store nil buffer-A buffer-B #'point-max)
      (with-current-buffer buffer-B
	(expect (buffer-string) :to-equal "foo-bar-foo"))))

  (it "saves only the target buffer"
    (spy-on #'save-buffer)
    (spy-on #'workspace-save-buffer)
    
    (let ((buffer-A (generate-new-buffer "foo"))
	  (buffer-B (generate-new-buffer "bar")))
      ;; when storing into a <where> buffer
      (workspace-store nil buffer-A buffer-B #'point-min)

      ;; then the <where> buffer was saved
      (expect #'workspace-save-buffer :to-have-been-called-with nil buffer-B)
      ;; and no other buffer was saved
      (expect #'save-buffer :not :to-have-been-called))))
