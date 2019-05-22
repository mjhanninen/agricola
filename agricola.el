;;; -*- lexical-binding: t -*-
;;; agricola.el -- Supporting library for Agricola color theme

;;; Copyright © Matti Hänninen 2017-19

(require 'generator)

;;;
;;; Miscallanea
;;;

;; XXX(soija) Hmm, the `loop` facility didn't support iterators/generators
;; although the documentation claimed otherwise.
(defun iter-drain (iterator)
  (let ((elems))
    (condition-case err
        (while t
          (setq elems (cons (iter-next iterator) elems)))
      (iter-end-of-sequence
       (reverse elems)))))

(defun agricola/new-spec ()
  (make-hash-table))

(comment
  (cl-defstruct (agricola--face-spec (:constructor agricola--make-face-spec)
                                     (:copier nil))
    isa props))

(defvar agricola--missing (gensym))

(defun agricola--get-or-make-face-spec (spec facesym)
  (let ((face-spec (gethash facesym spec agricola--missing)))
    (if (eq face-spec agricola--missing)
        (puthash facesym (make-hash-table) spec)
      face-spec)))

(iter-defun agricola--iter-property-clauses (forms)
  (let ((tail forms))
    (while (car tail)
      (let ((face-sym (car tail)))
        (if (symbolp face-sym)
            (let ((props))
              (setq tail (cdr tail))
              (while (keywordp (car tail))
                (setq props (cons (cons (car tail)
                                        (cadr tail))
                                  props))
                (setq tail (cddr tail)))
              (iter-yield (cons face-sym (reverse props))))
          (error "expected SYMBOL naming a face"))))))

(defun agricola--parse-free-properties (forms)
  (iter-drain (agricola--iter-property-clauses forms)))

(defun agricola--parse-declared-properties (prop-names forms)
  (let ((prop-count (length prop-names)))
    (seq-map #'(lambda (clause)
                 (let ((face-sym (car clause))
                       (prop-values (cdr clause)))
                   (if (= (length prop-values) prop-count)
                       (cons face-sym (seq-mapn #'cons prop-names prop-values))
                     (error "too values for properties"))))
             (seq-partition forms (1+ prop-count)))))

(defun agricola--form-inheritance-props (inheritance-chains)
  (seq-mapcat #'(lambda (inheritance-chain)
                  (seq-mapn #'(lambda (parent-face child-face)
                                (list child-face (cons :inherit parent-face)))
                            inheritance-chain
                            (cdr inheritance-chain)))
              inheritance-chains))

(defun agricola--apply-props (spec parsed-props)
  (dolist (clause parsed-props)
    (let ((face-spec (agricola--get-or-make-face-spec spec (car clause))))
      (dolist (prop (cdr clause))
        (puthash (car prop) (cdr prop) face-spec))))
  spec)

(defmacro agricola/set-property (spec &rest forms)
  (declare (indent 0))
  `(agricola--apply-props ,spec
                          (agricola--parse-free-properties (list ,@forms))))


(defmacro agricola/set-properties (spec properties &rest forms)
  (declare (indent 1))
  `(agricola--apply-props ,spec
                          (agricola--parse-declared-properties (list ,@properties)
                                                               (list ,@forms))))

(defmacro agricola/inherit (spec &rest forms)
  (declare (indent 0))
  `(agricola--apply-props ,spec
                          (agricola--form-inheritance-props
                           (list ,@(mapcar #'(lambda (inheritance-chain)
                                               `(list ,@inheritance-chain))
                                           forms)))))

(defun agricola/touch (spec &rest forms)
  (declare (indent 0))
  (dolist (face forms)
    (agricola--get-or-make-face-spec spec face))
  spec)

;;;; Building spec for `custom-theme-set-faces'

;; (FACE ((DISPLAY . PLIST)))

;; Checks if given key is present in the table.  This should be built-in.
(defun agricola--hashash (key table)
  (let ((missing (gensym)))
    (not (eq (gethash key table missing) missing))))

(defun agricola--compile-face-spec (face properties)
  ;; XXX(soija) For now use display `t` that matches *all* displays.  Think
  ;; about the terminal later.
  (let ((display t)
        (prop-list))
    (maphash #'(lambda (prop value)
                 (setq prop-list (append prop-list (list prop value))))
             properties)
    (list face (list (cons display prop-list)))))

(defun agricola/compile-spec (data)
  (let ((spec))
    (maphash #'(lambda (face props)
                 (setq spec
                       (cons (agricola--compile-face-spec face props)
                             spec)))
             data)
    (reverse spec)))

;;;;

;; XXX(soija) Consider using `color-rgb-to-hex'
(defun agricola/rgb (r g b)
  (format "#%02x%02x%02x" r g b))

(defun agricola--make-color-definition (spec)
  (cl-destructuring-bind (s r g b) spec
    `(setq ,s ,(agricola/rgb r g b))))

(defun agricola--spec-to-definitions (specs)
  `(progn
     ,@(mapcar #'agricola--make-color-definition
               (seq-partition specs 4))
     nil))

(defmacro agricola/defcolor (&rest specs)
  (declare (indent 0))
  (agricola--spec-to-definitions specs))

;;;; Namespace or prefix abbreviations

(defun agricola--symbol-prefix (sym)
  (let ((parts (split-string (symbol-name sym) "/")))
    (when (and (> (length parts) 1)
             (car parts))
      (car parts))))

(defun agricola--replace-symbol-prefix (sym replacement)
  (let ((parts (split-string (symbol-name sym) "/")))
    (if (and (> (length parts) 1)
             (car parts))
        (intern (string-join (cons replacement (cdr parts)) "/"))
      sym)))

(defun agricola--expand-single-abbr-in-symbol (abbr expansion symbol)
  (let ((prefix (agricola--symbol-prefix symbol)))
    (if (string= prefix abbr)
        (agricola--replace-symbol-prefix symbol expansion)
      symbol)))

(defun agricola--expand-single-abbr (abbr expansion body)
  (cond
   ((symbolp body) (agricola--expand-single-abbr-in-symbol abbr expansion body))
   ((listp body) (mapcar #'(lambda (e)
                             (agricola--expand-single-abbr abbr expansion e))
                         body))
   (t body)))

(defun agricola--expand-abbreviations (abbrs body)
  (seq-reduce #'(lambda (body abbr-rule)
                  (let ((abbr (symbol-name (car abbr-rule)))
                        (expansion (symbol-name (cadr abbr-rule))))
                    (agricola--expand-single-abbr abbr expansion body)))
              abbrs
              body))

(defmacro agricola/with-abbreviated-prefix (abbrs &rest body)
  (declare (indent 1))
  `(progn
     ,@(agricola--expand-abbreviations abbrs body)))

(provide 'agricola)
