(setq hash (make-hash-table))
(puthash 'redirect_to "Hash | Record | String | Proc | :back, {:status :flash :notice :alert}" hash)
(puthash 'test_one_arg "string" hash)

(setq show-args-overlay nil)

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
  (insert "  ")
  (backward-char 2))

(defun show-args-create-overlay-at-point ()
  (show-args-create-a-space-at-point)
  (setq show-args-overlay (make-overlay (+ 1 (point)) (+ 2 (point))))
  (put-text-property (point) (+ 1 (point)) 'insert-in-front-hooks '(show-args-abort-if-not-space-or-open-paren)))

(defun show-args-create-functions-overlay-at-point ()
  (interactive)
  (show-args-cleanup)
  (show-args-create-overlay-at-point)
  (overlay-put show-args-overlay 'font-lock-face 'show-args-face)
  (overlay-put show-args-overlay 'display (show-args-for-at-point))
  (overlay-put show-args-overlay 'insert-in-front-hooks '(show-args-insert-key-hook)))

(defun show-args-insert-key-hook(overlay after begin end &optional length-replaced)
  (if after
      (progn
        (if (show-args-did-hit-space-or-comma overlay begin end)
            (show-args-delete-first-char-in-overlay overlay)
          (show-args-move-overlay-foward-one overlay)
          (overlay-put overlay 'display (show-args-remove-up-to-first-comma))
          (if (show-args-overlay-empty overlay)
              (show-args-cleanup)
            (overlay-put overlay 'display (concat ", " (overlay-get overlay 'display))))))))


(defun show-args-overlay-empty(overlay)
  (eq 0 (length (overlay-get overlay 'display))))

(defun show-args-delete-first-char-in-overlay(overlay)
  (overlay-put overlay 'display (show-args-remove-the-first-overlay-char overlay))
  (show-args-move-overlay-foward-one overlay))

(defun show-args-move-overlay-foward-one(overlay)
  (move-overlay overlay (+ 1 (overlay-start overlay)) (+ 1 (overlay-end overlay))))

(defun show-args-did-hit-space-or-comma(overlay begin end)
  (or 
   (and (string= "," (buffer-substring-no-properties begin end)) (string= "," (show-args-first-char-in-overlay overlay)))
   (and (string= " " (buffer-substring-no-properties begin end)) (string= " " (show-args-first-char-in-overlay overlay)))))

(defun show-args-first-char-in-overlay(overlay)
  (substring (overlay-get overlay 'display) 0 1))

(defun show-args-remove-the-first-overlay-char(overlay)
  (overlay-put overlay 'display (substring (overlay-get overlay 'display) 1)))


(defun show-args-remove-up-to-first-comma()
  (mapconcat 'identity (cdr (split-string (overlay-get overlay 'display) ", ")) ", "))

(defun show-args-abort-if-not-space-or-open-paren(begin end) 
  (if (not (or 
       (string= " " (buffer-substring-no-properties begin end))
       (string= "(" (buffer-substring-no-properties begin end))))
      (progn
        (show-args-cleanup)
        (remove-insert-hook-text-property 'show-args-abort-if-not-space-or-open-paren))
    (delete-char 1)))

(defun remove-insert-hook-text-property(hook-name) 
  (put-text-property (point) (+ 1 (point)) 'insert-in-front-hooks '())) ; removes all

(defun show-args-cleanup()
  (if (overlayp show-args-overlay)
      (delete-overlay show-args-overlay)))
