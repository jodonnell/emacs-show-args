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
  (show-args-create-overlay-at-point)
  (overlay-put show-args-overlay 'font-lock-face 'show-args-face)
  (overlay-put show-args-overlay 'display (show-args-for-at-point))
  (overlay-put show-args-overlay 'insert-in-front-hooks '(show-args-insert-key-hook)))

(defun show-args-insert-key-hook(overlay after begin end &optional length-replaced)
  (if after
      (progn
        (if (show-args-did-hit-space-or-comma overlay begin end)
            (progn
              (overlay-put overlay 'display (show-args-remove-the-first-overlay-char overlay))
              (move-overlay overlay (+ 1 (overlay-start overlay)) (+ 1 (overlay-end overlay))))
          (move-overlay overlay (+ 1 (overlay-start overlay)) (+ 1 (overlay-end overlay)))
          (overlay-put overlay 'display (show-args-remove-up-to-first-comma))))))


(defun show-args-did-hit-space-or-comma(overlay begin end)
  (or 
   (and (string= "," (buffer-substring-no-properties begin end)) (string= "," (show-args-first-char-in-overlay overlay)))
   (and (string= " " (buffer-substring-no-properties begin end)) (string= " " (show-args-first-char-in-overlay overlay)))))

(defun show-args-first-char-in-overlay(overlay)
  (substring (overlay-get overlay 'display) 0 1))

(defun show-args-remove-the-first-overlay-char(overlay)
  (overlay-put overlay 'display (substring (overlay-get overlay 'display) 1)))


(defun show-args-remove-up-to-first-comma()
  (concat ", " (mapconcat 'identity (cdr (split-string (overlay-get overlay 'display) ", ")) ", ")))

(defun show-args-abort-if-not-space-or-open-paren(begin end) 
  (if (not (or 
       (string= " " (buffer-substring-no-properties begin end))
       (string= "(" (buffer-substring-no-properties begin end))))
      (progn
        (delete-overlay show-args-overlay)
        (remove-insert-hook-text-property 'show-args-abort-if-not-space-or-open-paren))
    (delete-char 1)))

(defun remove-insert-hook-text-property(hook-name) 
  (put-text-property (point) (+ 1 (point)) 'insert-in-front-hooks '())) ; removes all

