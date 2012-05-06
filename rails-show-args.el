(setq hash (make-hash-table))
(puthash 'redirect_to "Hash | Record | String | Proc | :back, {:status :flash :notice :alert}" hash)
(puthash 'test_one_arg "string" hash)

(setq sa-overlay nil)

(defvar show-args-mode nil
  "Dummy variable to suppress compiler warnings.")

(defvar show-args-mode-hook nil
  "Hook for `show-args-mode'.")

(defface sa-face
  '((t (:foreground "darkgray" :underline t)))
  "Face for show args"
  :group 'show-args)

(define-minor-mode show-args-mode
  "Show Argument mode"
  :lighter " SA"
  :group 'show-args
  (if show-args-mode
      (progn
        (add-hook 'post-command-hook 'sa-show-args-if-function nil t)
        (add-hook 'pre-command-hook 'sa-cleanup-if-not-self-insert nil t)
        (run-hooks 'show-args-mode-hook))
    (remove-hook 'pre-command-hook 'sa-cleanup-if-not-self-insert t)
    (remove-hook 'post-command-hook 'sa-show-args-if-function t)))


(defun sa-cleanup-if-not-self-insert()
  (if (not (eq this-command 'self-insert-command))
      (sa-cleanup)))

(defun sa-for (function)
  (sa-for-symbol (intern function)))

(defun sa-for-symbol (function)
  (gethash function hash))

(defun sa-for-at-point ()
  (sa-for (thing-at-point 'symbol)))

(defun sa-is-known-function-at-point()
  (not (eq nil (gethash (intern (thing-at-point 'symbol)) hash))))

(defun sa-create-two-spaces-at-point ()
  (insert "  ")
  (backward-char 2))

(defun sa-create-overlay-at-point ()
  (setq sa-overlay (make-overlay (+ 1 (point)) (+ 2 (point))))
  (overlay-put sa-overlay 'font-lock-face 'sa-face)
  (overlay-put sa-overlay 'display (sa-for-at-point))
  (overlay-put sa-overlay 'insert-in-front-hooks '(sa-overlay-insert-key-hook)))
  
(defun sa-create-text-property()
  (put-text-property (point) (+ 1 (point)) 'rear-nonsticky t)
  (put-text-property (point) (+ 1 (point)) 'sa-text-property t)
  (put-text-property (point) (+ 1 (point)) 'insert-in-front-hooks '(sa-abort-if-not-space-or-open-paren)))

(defun sa-show-args-if-function()
  (if (and (eq this-command 'self-insert-command)
           (> (length (thing-at-point 'symbol)) 0)
           (sa-is-known-function-at-point))
      (sa-create-functions-overlay-at-point)))

(defun sa-create-functions-overlay-at-point ()
  (sa-cleanup)
  (sa-create-two-spaces-at-point)
  (sa-create-text-property)
  (sa-create-overlay-at-point))

(defun sa-overlay-insert-key-hook(overlay after begin end &optional length-replaced)
  (if after
      (sa-overlay-key-hit overlay begin end)))

(defun sa-overlay-key-hit(overlay begin end)
  (if (sa-did-hit-space-or-comma overlay begin end)
      (sa-delete-first-char-in-overlay overlay)
    (sa-remove-part-of-overlay overlay begin end)))

(defun sa-remove-part-of-overlay(overlay begin end)
  (sa-move-overlay-foward-one overlay)
  (overlay-put overlay 'display (sa-remove-up-to-first-comma))
  (if (sa-overlay-empty overlay)
      (sa-cleanup)
    (overlay-put overlay 'display (concat ", " (overlay-get overlay 'display)))))

(defun sa-overlay-empty(overlay)
  (eq 0 (length (overlay-get overlay 'display))))

(defun sa-delete-first-char-in-overlay(overlay)
  (sa-remove-the-first-overlay-char overlay)
  (sa-move-overlay-foward-one overlay))

(defun sa-move-overlay-foward-one(overlay)
  (move-overlay overlay (+ 1 (overlay-start overlay)) (+ 2 (overlay-start overlay))))

(defun sa-did-hit-space-or-comma(overlay begin end)
  (or 
   (and (string= "," (buffer-substring-no-properties begin end)) (string= "," (sa-first-char-in-overlay overlay)))
   (and (string= " " (buffer-substring-no-properties begin end)) (string= " " (sa-first-char-in-overlay overlay)))))

(defun sa-first-char-in-overlay(overlay)
  (substring (overlay-get overlay 'display) 0 1))

(defun sa-remove-the-first-overlay-char(overlay)
  (overlay-put overlay 'display (substring (overlay-get overlay 'display) 1)))

(defun sa-remove-up-to-first-comma()
  (mapconcat 'identity (cdr (split-string (overlay-get overlay 'display) ", ")) ", "))

(defun sa-abort-if-not-space-or-open-paren(begin end) 
  (if (not (or 
            (string= " " (buffer-substring-no-properties begin end))
            (string= "(" (buffer-substring-no-properties begin end))))
      (sa-cleanup)
    (delete-char 1)))

(defun sa-cleanup()
  (let ((my-text-prop-at (text-property-any (point-min) (point-max) 'show-args-text-property t)))
    (if my-text-prop-at
        (sa-cleanup-text-property my-text-prop-at)))

  (if (overlayp sa-overlay)
      (delete-overlay sa-overlay)))

(defun sa-cleanup-text-property(text-prop-at)
  (put-text-property text-prop-at (+ 1 text-prop-at) 'rear-nonsticky nil)
  (put-text-property text-prop-at (+ 1 text-prop-at) 'show-args-text-property nil)
  (put-text-property text-prop-at (+ 1 text-prop-at) 'insert-in-front-hooks '()))
