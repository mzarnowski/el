;;; -*- lexical-binding: t; -*-

(require 'buttercup)
(require 'dash)

(require 'workflow)
(require 'workspace)
(require 'randomize-workspace)

(require 'workspace-capture)
(require 'workspace-inbox)


(describe "inbox"
  (before-each
    (spy-on #'workspace-select :and-return-value (random-workspace))

    (-each '("Foo" "Bar" "Baz") (lambda (title)
				  (with-current-buffer (window-buffer (workspace-capture))
				    (insert title)
				    (workflow-continue)))))

  (it "finds all entries"
    (let* ((entries (workspace-inbox-entries (workspace-select)))
	   (names (-map #'workspace-inbox-entry-title entries)))
      (expect names :to-equal (list "Foo" "Bar" "Baz")))))
