(in-package #:scanfcl)

(defun sscanf (input-string control-string)
  "Parses formatted input text, reading characters from *input-string* and converting sequences of characters according to the *control-string* format. The *control-string* can be a string or a *compiled control string* (see `COMPILE-CONTROL-STRING`). Returns the items converted from the *input-string* as a list."
  (with-input-from-string (*standard-input* input-string)
    (scanf control-string)))

(defun fscanf (input-stream control-string)
  "Parses formatted input text, reading characters from *input-stream* and converting sequences of characters according to the *control-string* format. The *control-string* can be a string or a *compiled control string* (see `COMPILE-CONTROL-STRING`). Returns the items converted from the *input-stream* as a list."
  (let ((*standard-input* input-stream))
    (scanf control-string)))

(defparameter *result* nil
  "The results of scanning `*string*` so far.")

(defun scanf (control-string)
  "Parses formatted input text, reading characters from `*STANDARD-INPUT*` and converting sequences of characters according to the *control-string* format. The *control-string* can be a string or a *compiled control string* (see `COMPILE-CONTROL-STRING`). Returns the items converted from `*STANDARD-INPUT*` as a list."
  (let ((control-string
          (etypecase control-string
            (string (compile-control-string control-string))
            (function control-string)))
        (*result* '()))
    (funcall control-string)
    (values (reverse *result*) (file-position *standard-input*))))

(defun compile-control-string (control-string)
  "Returns a function suitable for passing to `SCANF`, `SSCANF` and `FSCANF`."
  (compile nil (create-scanner control-string)))

;;; conversion specification
;;; #\% followed by:
;;; 1. Optional *assignment suppression flag* #\*
;;; 2. Optional maximum *field width* d+
;;; 3. Optional *length modifier* hh, h, l, ll, j, z, t, L
;;; 4. Required *conversion specifier*
;;;    a, c, d, e, f, g, i, n, o, p, s, u, x, %, [ - we ignore n and p]

(defgeneric collect-length-modifier (converter control-string control-string-index)
  (:documentation "Return two values: a representation of the *length modifier* (if any) in `control-string` starting at `control-string-index`; and the updated value of `control-string-index`."))

(defgeneric collect-conversion-specifier (converter control-string control-string-index)
  (:documentation "Return two values: a representation of the *conversion specifier* in `control-string` starting at `control-string-index`; and the updated value of `control-string-index`."))

(defgeneric collect-scanset (converter control-string control-string-index)
  (:documentation "Return two values: a two-item list of a *scanset* (suitable for consumption by `MAKE-CONVERSION-SCANNER` as part of a *conversion-specifier*) and a boolean indicating whether the scanset is negated; and the updated value of `control-string-index`."))

(defgeneric collect-field-width (converter control-string control-string-index)
  (:documentation "Return two values: the *field width* (if any) specified in `control-string` starting at `control-string-index`; and the udpated value of `control-string-index`."))

(defgeneric make-conversion-scanner (converter conversion-specifier suppressp field-width length-modifier)
  (:documentation "Return a scanner, a function of no arguments returning an appropriate value from `*string*` given the arguments."))

(define-condition scanfcl-error (error)
  ((conversion-specifier :reader conversion-specifier :initarg :conversion-specifier)
   (reason :reader reason :initarg :reason))
  (:report (lambda (condition stream)
             (format stream "when converting '~A', ~A"
                     (conversion-specifier condition)
                     (reason condition)))))

(defclass standard-converter ()
  ())

(defparameter *converter* (make-instance 'standard-converter))

(defmethod collect-field-width ((converter standard-converter) control-string control-string-index)
  (let ((field-width nil))
    (loop with result = 0
          for char = (char control-string control-string-index)
          for val = (digit-char-p char)
          while val
          do (setf result (+ (* result 10) val))
             (incf control-string-index)
          finally (unless (zerop result)
                    (setf field-width result)))
    (values field-width control-string-index)))

(defmethod collect-length-modifier ((converter standard-converter) control-string control-string-index)
  (let ((char (char control-string control-string-index)))
    (case char
      ((#\j #\z #\t #\L)
       (values (string char) (incf control-string-index)))
      (#\l
       (incf control-string-index)
       (let ((second (char control-string control-string-index)))
         (if (char= #\l second)
             (values "ll" (incf control-string-index))
             (values "l" control-string-index))))
      (#\h
       (incf control-string-index)
       (let ((second (char control-string control-string-index)))
         (if (char= #\h second)
             (values "hh" (incf control-string-index))
             (values "h" control-string-index))))
      (t
       (values nil control-string-index)))))

(defmethod collect-scanset ((converter standard-converter) control-string control-string-index)
 (let ((negation nil)
        (scanset '())
        (first (char control-string control-string-index)))
    (when (char= #\^ first)
      (setf negation t)
      (incf control-string-index)
      (setf first (char control-string control-string-index)))
    (push first scanset)
    (incf control-string-index)
    (loop for char = (char control-string control-string-index)
          until (char= #\] char)
          do (pushnew char scanset :test #'char=)
             (incf control-string-index))
    (values (list scanset negation) (incf control-string-index))))

(defmethod collect-conversion-specifier ((converter standard-converter)
                                         control-string control-string-index)
  (let ((char (char control-string control-string-index)))
    (if (not (char= #\[ char))
        (values (intern (string char) :keyword) (incf control-string-index))
        (multiple-value-bind (scanset index)
            (collect-scanset *converter* control-string (incf control-string-index))
          (values (list :|[| scanset) index)))))

(defun handle-conversion-specification (control-string control-string-index)
  (let ((suppressp nil)
        (field-width nil)
        (length-modifier '())
        (conversion-specifier nil))
    (when (char= #\* (char control-string control-string-index))
      (setf suppressp t)
      (incf control-string-index))
    (multiple-value-bind (fw index)
        (collect-field-width *converter* control-string control-string-index)
      (setf field-width fw
            control-string-index index))
    (multiple-value-bind (lm index)
        (collect-length-modifier *converter* control-string control-string-index)
      (setf length-modifier lm
            control-string-index index))
    (multiple-value-bind (cs index)
        (collect-conversion-specifier *converter* control-string control-string-index)
      (setf conversion-specifier cs
            control-string-index index))
    (values (make-conversion-scanner
             *converter* conversion-specifier suppressp field-width length-modifier)
            control-string-index)))

(defmethod make-conversion-scanner ((converter standard-converter) (conversion-specifier cons)
                                    suppressp field-width length-modifier)
  (destructuring-bind (symbol (scanset negationp)) conversion-specifier
    (case symbol
      (:|[| (make-scanset-scanner scanset negationp suppressp field-width length-modifier))
      (t (error "Unknown conversion specifier: '~A'" symbol)))))

(defmethod make-conversion-scanner ((converter standard-converter) (cs (eql :|d|))
                                     suppressp field-width length-modifier)
  (make-integer-scanner suppressp field-width length-modifier :radix 10))

(defmethod make-conversion-scanner ((converter standard-converter) (cs (eql :|o|))
                                    suppressp field-width length-modifier)
  (make-integer-scanner suppressp field-width length-modifier :radix 8))

(defmethod make-conversion-scanner ((converter standard-converter) (cs (eql :|u|))
                                    suppressp field-width length-modifier)
  (make-integer-scanner suppressp field-width length-modifier :radix 10))

(defmethod make-conversion-scanner ((converter standard-converter) (cs (eql :|s|))
                                    suppressp field-width length-modifier)
  (make-string-scanner suppressp field-width length-modifier))

(defmethod make-conversion-scanner ((converter standard-converter) (cs (eql :|x|))
                                    suppressp field-width length-modifier)
  (make-integer-scanner suppressp field-width length-modifier :radix 16))

(defmethod make-conversion-scanner ((converter standard-converter) (cs (eql :|c|))
                                    suppressp field-width length-modifier)
  (make-character-scanner suppressp field-width length-modifier))

(defmethod make-conversion-scanner ((converter standard-converter) (cs (eql :|i|))
                                    suppressp field-width length-modifier)
  (make-signed-integer-scanner suppressp field-width length-modifier))

(defmethod make-conversion-scanner ((converter standard-converter) (cs (eql :|a|))
                                    suppressp field-width length-modifier)
  (make-floating-point-scanner suppressp field-width length-modifier))

(defmethod make-conversion-scanner ((converter standard-converter) (cs (eql :|f|))
                                    suppressp field-width length-modifier)
  (make-floating-point-scanner suppressp field-width length-modifier))

(defmethod make-conversion-scanner ((converter standard-converter) (cs (eql :|e|))
                                    suppressp field-width length-modifier)
  (make-floating-point-scanner suppressp field-width length-modifier))

(defmethod make-conversion-scanner ((converter standard-converter) (cs (eql :|g|))
                                    suppressp field-width length-modifier)
  (make-floating-point-scanner suppressp field-width length-modifier))

;;;;
#|

(defparameter *string* nil
  "The string being scanned.")

(defparameter *index* nil
  "The point within `*string*` to which scanning has proceeded.")

(defparameter *limit* nil
  "The limiting index of `*string*`")

(defun forward ()
  (incf *index*))

(defun current ()
  (when (< *index* *limit*)
    (char *string* *index*)))

(defun looking-at (char)
  (char= char (char *string* *index*)))
|#

(defun forward ()
  (read-char *standard-input* nil nil))

(defun current ()
  (peek-char nil *standard-input* nil nil))

(defun looking-at (char)
  (let ((peek (peek-char nil *standard-input* nil nil)))
    (and peek (char= char peek))))

(defparameter *whitespace* (coerce (list #\Space #\Newline #\Return #\Tab #\Vt #\Ff) 'string))

(defun isspace (char)
  (when char
    (find char *whitespace* :test #'char=)))

(defun isnanbody (char)
  (when char
    (or (alphanumericp char)
        (char= #\_ char))))


(defun skip-ws ()
  `(loop for char = (current)
         while (isspace char)
         do (forward)))

(defun make-char-matcher (char)
  `(if (looking-at ,char)
         (forward)
         (return (reverse *result*))))

(defun make-integer-scanner (suppressp field-width length-modifier &key radix)
  (declare (ignore length-modifier))
  `(progn
     ,(skip-ws)
     (let ((sign 1)
           (result 0)
           (limit ,(if field-width field-width -1)))
       (labels ((forward* () (forward) (decf limit)))
         (cond ((looking-at #\+)
                (forward*))
               ((looking-at #\-)
                (setf sign -1)
                (forward*)))
         ,@(when (and radix (= radix 16))
             '((when (looking-at #\0)
                 (forward*)
                 (when (or (looking-at #\x)
                           (looking-at #\X))
                   (forward*)))))
         (if (or (null (current)) (digit-char-p (current) ,radix))
             (loop for char = (current)
                   until (or (null char)
                             (not (digit-char-p char ,radix))
                             (zerop limit))
                   do (setf result (+ (* result ,radix) (digit-char-p char ,radix)))
                      (forward*)
                   finally ,(if suppressp '(values) '(push (* sign result) *result*)))
             (return (reverse *result*)))))))

(defun make-signed-integer-scanner (suppressp field-width length-modifier)
  (declare (ignore length-modifier))
  `(progn
     ,(skip-ws)
     (let ((sign 1)
           (radix 10)
           (result 0)
           (limit ,(if field-width field-width -1)))
       (labels ((forward* () (forward) (decf limit)))
         (cond ((looking-at #\+)
                (forward*))
               ((looking-at #\-)
                (setf sign -1)
                (forward*)))
         (when (looking-at #\0)
           (forward*)
           (setf radix 8)
           (when (or (looking-at #\x)
                     (looking-at #\X))
             (forward*)
             (setf radix 16)))
         (if (or (null (current)) (digit-char-p (current) radix))
             (loop for char = (current)
                   until (or (null char)
                             (not (digit-char-p char radix))
                             (zerop limit))
                   do (setf result (+ (* result radix) (digit-char-p char radix)))
                      (forward*)
                   finally ,(if suppressp '(values) '(push (* sign result) *result*)))
             (return (reverse *result*)))))))

(defun make-floating-point-scanner (suppressp field-width length-modifier)
  `(progn
     ,(skip-ws)
     (let ((sign 1d0)
           (limit ,(if field-width field-width -1)) ; FIXME - hacky
           (saw-something-p nil)
           (result nil))
       (labels ((forward* ()
                  (forward)
                  (decf limit)
                  (setf saw-something-p t)))
         (cond ((looking-at #\+)
                (forward*))
               ((looking-at #\-)
                (setf sign -1.0)
                (forward*)))
         (cond ((or (looking-at #\I)
                    (looking-at #\i))
                (loop for char = (current)
                   for match across "INFINITY"
                   for match-count from 1
                   until (or (null char)
                             (zerop limit)
                             (not (char-equal char match)))
                   do (forward*)
                   finally (if (or (= 3 match-count)
                                   (= 8 match-count))
                               (if (plusp sign)
                                   (setf result (positive-infinity ,length-modifier))
                                   (setf result (negative-infinity ,length-modifier)))
                               (return (reverse *result*)))))
               ((or (looking-at #\N)
                    (looking-at #\n))
                (loop for char = (current)
                   for match across "NAN"
                   for match-count from 1
                   until (or (null char)
                             (zerop limit)
                             (not (char-equal char match)))
                   do (forward*)
                   finally (if (= 3 match-count)
                               (progn
                                 (when (looking-at #\()
                                   (forward*)
                                   (loop for char = (current)
                                      while (and (isnanbody char) (not (zerop limit)))
                                      do (forward*))
                                   (if (looking-at #\) )
                                       (forward*)
                                       (return (reverse *result*))))
                                 (setf result (nan ,length-modifier)))
                               (return (reverse *result*)))))
               (t
                (let ((significand 0)
                      (exponent 0)
                      (exponent-sign 1)
                      (exponent-marker #\e)
                      (radix 10))
                  (when (and (not (zerop limit))
                             (looking-at #\0))
                    (forward*)
                    (when (and (not (zerop limit))
                               (or (looking-at #\x)
                                   (looking-at #\X)))
                      (forward*)
                      (setf radix 16
                            exponent-marker #\p)))
                  (loop for char = (current)
                     until (or (null char)
                               (zerop limit)
                               (not (digit-char-p char radix)))
                     do (setf significand
                              (+ (* significand radix) (digit-char-p char radix)))
                       (forward*))
                  (when (and (not (zerop limit))
                             (looking-at #\.))
                    (forward*))
                  (loop for char = (current)
                     for exp downfrom -1
                     until (or (null char)
                               (zerop limit)
                               (not (digit-char-p char radix)))
                     do (setf significand
                              (+ significand (* (digit-char-p char radix)
                                                (expt radix exp))))
                       (forward*))
                  (when (and (not (zerop limit))
                             (current)
                             (char-equal exponent-marker (current)))
                    (forward*)
                    (cond ((looking-at #\+)
                           (forward*))
                          ((looking-at #\-)
                           (setf exponent-sign -1)
                           (forward*)))
                    (loop for char = (current)
                       until (or (null char)
                                 (not (digit-char-p char))
                                 (zerop limit))
                       do (setf exponent
                                (+ (* exponent 10) (digit-char-p char)))
                           (forward*)))
                  (if saw-something-p
                      (setf result
                            (* sign
                               significand
                               (expt (if (= radix 16) 2 10)
                                     (* exponent-sign exponent))))
                      (return (reverse *result*))))))
         ,(if suppressp
              '(values)
              '(push result *result*))))))

(defun positive-infinity (length-modifier)
 (cond ((null length-modifier)
        float-features:single-float-positive-infinity)
       ((string= "l" length-modifier)
        float-features:double-float-positive-infinity)
       ((string= "L" length-modifier)
        float-features:long-float-positive-infinity)
       (t
        (warn "Inapplicable length modifier for float: '~A'" length-modifier)
        float-features:double-float-positive-infinity)))

(defun negative-infinity (length-modifier)
  (cond ((null length-modifier)
         float-features:single-float-negative-infinity)
        ((string= "l" length-modifier)
         float-features:double-float-negative-infinity)
        ((string= "L" length-modifier)
         float-features:long-float-negative-infinity)
        (t
         (warn "Inapplicable length modifier for float: '~A'" length-modifier)
         float-features:double-float-negative-infinity)))

;; 8:52 PM <Krystof> splittist: You probably need to remove :invalid from float-traps
;; 8:52 PM <Krystof> then sb-kernel:make-single-float <bits> (and sb-kernel:make-double-float <bits> <more bits>)

(defun nan (length-modifier)
  (float-features:with-float-traps-masked (:invalid)
    (cond ((null length-modifier)
           (float-features:bits-single-float #x7FC00000))
          ((string= "l" length-modifier)
           (float-features:bits-double-float #x7FF8000000000000))
          ((string= "L" length-modifier)
           ;; no-one supports long-float like this (yet?)
           #+(or)(float-features:bits-long-float #x7FFF8000000000000000000000000000)
           ;; so we use a double-float instead
           (float-features:bits-double-float #x7FF8000000000000))
          (t
           (warn "Inapplicble length modifier for float: '~A'" length-modifier)
           (float-features:bits-double-float #x7FF8000000000000)))))

(defun make-scanset-scanner (scanset negationp suppressp field-width length-modifier)
  (declare (ignore length-modifier))
  `(let ((result '()))
     (loop for char = (current)
           ,@(when field-width '(for count from 1))
           until (or (null char)
                     ,@(when field-width `((> count ,field-width)))
                     ,(if negationp
                          `(member char ',scanset)
                          `(not (member char ',scanset))))
           do (push char result)
              (forward)
           finally ,(if suppressp
                        '(values)
                        '(push (coerce (reverse result) 'string) *result*)))))

(defun make-string-scanner (suppressp field-width length-modifier)
  (declare (ignore length-modifier))
  `(progn
     ,(skip-ws)
     (if (not (isspace (current)))
         (let ((result '()))
           (loop for char = (current)
                 ,@(when field-width '(for count from 1))
              until (or (null char)
                        (isspace char)
                        ,@(when field-width `((> count ,field-width))))
                 do (push char result)
                    (forward)
                 finally ,(if suppressp
                              '(values)
                              '(push (coerce (reverse result) 'string) *result*))))
         (return (reverse *result*))))) ; FIXME or EOF?

(defun make-character-scanner (suppressp field-width length-modifier)
  (declare (ignore length-modifier))
  `(if (current)
       (let ((result '())
             (field-width (or ,field-width 1)))
         (loop for char = (current)
               for count from 1
               until (or (null char)
                         (> count field-width))
               do (push char result)
                  (forward)
               finally ,(if suppressp
                            '(values)
                            '(push (make-array (length result)
                                    :initial-contents (reverse result)
                                    :element-type 'character)
                              *result*))))
       (return (reverse *result*))))

(defun create-scanner (control-string)
  (let ((forms '())
        (control-string-index 0))
    (loop while (< control-string-index (length control-string))
          for char = (char control-string control-string-index)
          do (cond ((char= #\% char)
                    (incf control-string-index)
                    (let ((next (char control-string control-string-index)))
                      (if (char= #\% next)
                          (progn
                            (push (make-char-matcher #\%) forms)
                            (incf control-string-index))
                          (multiple-value-bind (form index)
                              (handle-conversion-specification
                               control-string control-string-index)
                            (push form forms)
                            (setf control-string-index index)))))
                   ((isspace char)
                    (incf control-string-index)
                    (when (< control-string-index (length control-string))
                      (loop while (and (< control-string-index (length control-string))
                                       (isspace (char control-string control-string-index)))
                            do (incf control-string-index)))
                    (push (skip-ws) forms))
                   (t
                    (incf control-string-index)
                    (push (make-char-matcher char) forms))))
    `(lambda ()
       (block nil
         ,@(reverse forms)))))


;;;;

(defparameter *system-v*
  '(;(:_Bool 1)
    (:char 1)
    (:unsigned-char 1)
    (:short 2)
    (:unsigned-short 2)
    (:int 4)
    (:unsigned-int 4)
    (:long 4)
    (:unsigned-long 4)
    (:long-long  8)
    (:unsigned-long-long 8)
;    (:pointer 4)
    (:float 4 single-float)
    (:double 8 double-float)
    (:long-double 8 double-float) ; or 12
    (:__float80 12 long-float)
    (:__float128 16 long-float)))

(defparameter *arm64*
  '((:unsigned-byte 1)
    (:signed-byte 1)
    (:unsigned-half-word 2)
    (:signed-half-word 2)
    (:unsigned-word 4)
    (:signed-word 4)
    (:unsigned-doube-word 8)
    (:signed-double-word 8)
    (:unsigned-quad-word 16)
    (:signed-quad-word 16)
;    (:pointer 8) ; or 4 beta
    (:half-precision 2 short-float) ; ?
    (:single-precision 4 single-float)
    (:double-precision 8 double-float)
    (:quad-precision  16 long-float)))

(defun is-float (type &optional (abi *system-v*))
  (third (assoc type abi)))

(defun is-unsigned (type)
  (= 8 (mismatch (symbol-name :unsigned) (symbol-name type))))

(defun bits (type &optional (abi *system-v*))
  (* 8 (second (assoc type abi))))

(defun convert-to-type (num type &optional (abi *system-v*))
  (cond ((and (floatp num) (is-float type abi))
         (convert-to-float num (is-float type abi)))
        ((floatp num)
         (convert-to-type (truncate num) type abi))
        ((is-float type abi)
         (convert-to-float num (is-float type abi)))
        ((is-unsigned type)
         (convert-to-unsigned num (bits type abi)))
        (t
         (convert-to-signed num (bits type abi)))))

(defun convert-to-float (num float-type)
  (ecase float-type
    (single-float
     (if (<= most-negative-single-float num most-positive-single-float)
         (float num 1s0)
         (if (plusp num)
             (positive-infinity nil)
             (negative-infinity nil))))
    (double-float
     (if (<= most-negative-double-float num most-positive-double-float)
         (float num 1d0)
         (if (plusp num)
             (positive-infinity "l")
             (negative-infinity "l"))))
    (long-float
     (if (<= most-negative-long-float num most-positive-long-float)
         (float num 1l0)
         (if (plusp num)
             (positive-infinity "L")
             (negative-infinity "L"))))))

(defun convert-to-unsigned (num bits)
  (mod num (expt 2 bits)))

(defun convert-to-signed (num bits)
  (if (< (- (1+ (expt 2 (1- bits)))) num (expt 2 (1- bits)))
      num
      (let ((base (logand num (1- (expt 2 bits)))))
        (if (< base (expt 2 (1- bits)))
            base
            (1+ (lognot base))))))


;;; Example
#|
/proc/net/unix format is defined in the unix_seq_show() function of the Linux kernel as:

"%pK: %08X %08X %08X %04X %02X %5lu" followed by a space and a path

(where 'K' is a special kernel format)

therefore we can scan with "%x: %8x %8x %4x %2x %5lu %s"

|#

;;; Tests https://github.com/llvm/llvm-project/blob/main/clang/test/Sema/format-strings-scanf.c

;;;;


(defclass my-converter (standard-converter)
  ())

(defmethod make-conversion-scanner ((converter my-converter) (cs (eql :|u|))
                                    suppressp field-width length-modifier)
  (let ((form (make-integer-scanner suppressp field-width length-modifier :radix 10)))
    (subst `(convert-to-type (* sign result) :unsigned-int) '(* sign result) form :test #'equal)))

(defun test-file (file)
  (let ((total 0)
        (passed 0)
        (failed 0)
        (skipped 0))
  (with-open-file (s file)
    (loop for line = (read-line s nil nil)
          while line
          do (unless (or (zerop (length line))
                         (char= #\# (char line 0)))
               (with-input-from-string (s line)
                 (let ((number (read s))
                       (input-string (read s))
                       (control-string (read s))
                       (results (read s)))
                   (incf total)
                   (handler-case
                       (let ((test-results
                               (sscanf input-string control-string)))
                         (if (equalp test-results results)
                             (progn
                               (princ #\Check_mark)
                               (princ #\Space)
                               (incf passed))
                             (progn
                               (format t "~%Failed:  ~D~
                                          ~%Control: ~S~
                                          ~%Input:   ~S~
                                          ~%Wanted:  ~S~
                                          ~%Got:     ~S~%"
                                       number control-string input-string
                                       results test-results)
                               (incf failed))))
                     (error (e)
                       (format t "~%Failed:  ~D~
                                  ~%Error: [~S] ~A~%"
                               number e e)
                       (incf failed))))))
          finally (format t "~%Tests ~D/~D; Passed ~D; Failed ~D~%"
                          total skipped passed failed)))))
