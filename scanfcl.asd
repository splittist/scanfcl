;;;; scanfcl.asd

(asdf:defsystem #:scanfcl
  :description "A c-like scanf implementation"
  :author "John Q. Splittist <splittist@splittist.com>"
  :license  "MIT"
  :version "0.0.1"
  :depends-on (#:float-features)
  :serial t
  :components ((:file "package")
               (:file "scanfcl")))

(asdf:defsystem #:scanfcl/test
  :depends-on (#:scanfcl)
  :serial t
  :components ((:file "test")))
