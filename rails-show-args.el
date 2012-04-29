(setq hash (make-hash-table))
(puthash 'redirect_to "Hash | Record | String | Proc | :back, {:status, :flash, :notice, :alert}"  hash)

(defface show-args-face
  '((t (:background "lightgray" :foreground "black")))
  "Face for candidate."
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

(defun show-args-create-two-spaces-at-point ()
  (insert "  ")
  (backward-char 2))

(defun show-args-create-overlay-at-point ()
  (show-args-create-two-spaces-at-point)
  (make-overlay (+ 1 (point)) (+ 2 (point))))

(defun show-args-create-functions-overlay-at-point ()
  (interactive)
  (let ((overlay (show-args-create-overlay-at-point)))
    (overlay-put overlay 'font-lock-face 'show-args-face)
    (overlay-put overlay 'display (show-args-for-at-point))))
