(defpackage #:css
  (:use :cl)
  (:export #:css))

(in-package :css)

(defun pprint-keyword (stream keyword)
  "Print lowercase KEYWORD to STREAM."
  (declare (keyword keyword))
  (let ((*print-pretty* nil))
    (format stream "~(~A~)" keyword)))

(defun pprint-selector-contents (stream contents)
  (write "{" :stream stream :pretty nil :escape nil)
  (prog1
      (let ((elements (loop for item in contents
                         unless (typep item 'css-selector-form)
                         collect item)))
        (pprint-selector-elements stream elements)
        (mapcan (lambda (item) (when (typep item 'css-selector-form) (list item)))
                contents))
    (write "}" :stream stream :pretty nil :escape nil)))

(defparameter *selectors* nil)

(defun pprint-selector (stream selector)
  (let ((*selectors* (append *selectors* (car selector))))
    (let ((*print-pretty* nil))
      (format stream "~{~(~A~) ~}" *selectors*))
    (loop for selectors in (pprint-selector-contents stream (cdr selector))
       do (print selectors stream))))

(defun pprint-selector-elements (stream elements)
  (when (consp elements)
    (if (listp (car elements))
        (progn (princ (car elements) stream)
               (pprint-selector-elements stream (nthcdr 1 elements)))
        (progn (princ (cons (car elements) (cadr elements)) stream)
               (pprint-selector-elements stream (nthcdr 2 elements))))))

(deftype selector-element ()
  '(cons keyword (not (or keyword cons))))
(deftype selector-element-with-list ()
  '(cons keyword (cons (member quote) cons)))

(deftype css-selector-form ()
  "The whole foo { ... } form.

In lisp this is ((:foo) ....)."
  '(or (cons (cons keyword)) (cons (cons (cons keyword)))))

(deftype selector-element-list ()
  "blah: 1 2 3

In lisp this is (:blah 1 2 3)."
  '(cons keyword (cons (not (or nil keyword)))))

(defun pprint-selector-element (stream selector)
  (declare (selector-element selector))
  (format stream "  ~A: ~A;~%" (car selector) (cdr selector)))

(defun pprint-selector-element-list (stream selector)
  (declare (selector-element-list selector))
  (format stream "  ~A: ~{~(~A~)~^ ~};" (car selector) (cdr selector)))

(defun pprint-selector-element-with-list (stream selector)
  (declare (selector-element-with-list selector))
  (format stream "  ~A: ~{~A~^, ~};" (car selector) (cadr (cdr selector))))

(defun pprint-string (stream string)
  "Print STRINGs to STREAM single quoted."
  (write "'" :stream stream :pretty nil)
  (write string :stream stream :pretty nil)
  (write "'" :stream stream :pretty nil))

(defun make-css-pprint-dispatch-table ()
  (let ((*print-pprint-dispatch* (copy-pprint-dispatch nil)))
    (set-pprint-dispatch 'keyword 'pprint-keyword)
    (set-pprint-dispatch 'css-selector-form
                         'pprint-selector 1)
    (set-pprint-dispatch 'selector-element
                         'pprint-selector-element)
    (set-pprint-dispatch 'selector-element-with-list
                         'pprint-selector-element-with-list)
    (set-pprint-dispatch 'string
                         'pprint-string)
    (set-pprint-dispatch 'selector-element-list
                         'pprint-selector-element-list)
    *print-pprint-dispatch*))

(defparameter *css-table* (make-css-pprint-dispatch-table))

(defun css-table ()
  *css-table*)

(defun call-with-css-pprint-table (thunk)
  (declare (type function thunk))
  (let ((*print-circle* nil)
        (*print-pprint-dispatch* (css-table)))
    (funcall thunk)))

(defmacro css ((&key (stream *standard-output*)) &body forms)
  "Print to *standard-output* css stylesheets."
  `(call-with-css-pprint-table
    (lambda () (mapc (lambda (form) (print form ,stream)) ',forms))))



;;; END
