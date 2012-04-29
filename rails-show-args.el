(setq hash (make-hash-table))
(puthash 'redirect_to "Hash | Record | String | Proc | :back, {:status, :flash, :notice, :alert}"  hash)

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
  (make-overlay (point) (+ 1 (point))))

(defun show-args-create-functions-overlay-at-point ()
  (let ((overlay (show-args-create-overlay-at-point)))  
    (overlay-put overlay 'display (show-args-for-at-point))))
