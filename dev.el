;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Utilities

(defun plist-put-non-nil (plist prop-name color)
  (if color
      (plist-put plist prop-name color)
    plist))

(defun agricola/insert-sample (foreground background)
  (let ((text "Sample text")
        (face-properties (thread-first '()
                           (plist-put-non-nil :foreground foreground)
                           (plist-put-non-nil :background background))))
    (add-text-properties 0
                         (length text)
                         `(face ,face-properties)
                         text)
    (insert text))
  (insert "\n"))

(defun unit (x)
  (/ x 255.0))

(defun byte (x)
  (round (* x 255.0)))

(defun to-xyz (r g b)
  (color-srgb-to-xyz (unit r)
                     (unit g)
                     (unit b)))

(defun from-xyz (x y z)
  (mapcar #'byte (color-xyz-to-srgb x y z)))

(defun to-hsl (r g b)
  (color-rgb-to-hsl (unit r)
                    (unit g)
                    (unit b)))

(defun from-hsl (h s l)
  (mapcar #'byte (color-hsl-to-rgb h s l)))
