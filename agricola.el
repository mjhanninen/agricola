;;; agricola.el -- Supporting library for Agricola color theme

;;; Copyright © Matti Hänninen 2017

(defun agricola/new-spec ())

(defun agricola/set-properties (spec properties & forms)
  "Updated "
  (declare (indent 1)))

(defun agricola/set-property (spec & forms)
  "Updated "
  (declare (indent 0)))

(defun agricola/inherit (spec & forms)
  (declare (indent 0)))

(defmacro agricola/with-prefix (replacements & forms)
  (declare (indent 1)))

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
