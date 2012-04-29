(setq hash (make-hash-table))
(puthash 'redirect_to "Hash | Record | String | Proc | :back, {:status, :flash, :notice, :alert}"  hash)

(defface show-args-face
  '((t (:foreground "darkgray" :underline t)))
  "Face for show args"
  :group 'show-args)


(defun show-args-for (function)
  (show-args-for-symbol (intern function)))

(defun show-args-for-symbol (function)
  (gethash function hash))

(defun show-args-for-at-point ()
  (interactive)
  (show-args-for (thing-at-point 'symbol)))

(defun show-args-function-docs-exist (function)
  (not (equal nil (gethash function hash))))

(defun show-args-create-a-space-at-point ()
  (insert " ")
  (backward-char 1))

(defun show-args-create-overlay-at-point ()
  (show-args-create-a-space-at-point)
  (make-overlay (point) (+ 1 (point))))

(defun show-args-create-functions-overlay-at-point ()
  (interactive)
  (let ((overlay (show-args-create-overlay-at-point)))
    (overlay-put overlay 'before-string " ")
    (overlay-put overlay 'after-string " ")
    (overlay-put overlay 'font-lock-face 'show-args-face)
    (overlay-put overlay 'display (show-args-for-at-point))))
