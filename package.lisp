(defpackage #:scanfcl
  (:use #:cl)
  (:export #:scanf
           #:sscanf
           #:fscanf

           #:create-scanner

           #:collect-length-modifier
           #:collect-conversion-specifier
           #:collect-field-width

           #:standard-converter

           #:*converter*))
