(setq hash (make-hash-table))
(puthash 'redirect_to "Hash | Record | String | Proc | :back, {:status, :flash, :notice, :alert}"  hash)

(defun rails-show-args-for (function)
  (rails-show-args-for-symbol (intern function)))

(defun rails-show-args-for-symbol (function)
  (gethash function hash))

(defun rails-show-args-for-at-point ()
  (interactive)
  (rails-show-args-for (thing-at-point 'symbol)))

(defun rails-show-args-function-docs-exist (function)
  (not (equal nil (gethash function hash))))

(defun rails-show-args-create-two-spaces-at-point ()
  (insert "  "))
