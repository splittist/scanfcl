(defpackage #:scanfcl
  (:use #:cl)
  (:export #:scanf
           #:sscanf
           #:fscanf

           #:compile-control-string

           #:create-scanner
           #:*result*

           #:collect-length-modifier
           #:collect-conversion-specifier
           #:collect-scanset
           #:collect-field-width
           #:make-conversion-scanner

           #:standard-converter

           #:*converter*))
