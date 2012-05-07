(defvar hash (make-hash-table))
(puthash 'redirect_to "Hash | Record | String | Proc | :back, {:status :flash :notice :alert}" hash)
(puthash 'form_for "record, {:url :namespace :html :method :format :as :remote}, &proc" hash)
(puthash 'render "{:action :layout :partial :template :file :text :xml :json :inline :js :nothing :update}, {:locals :status :object :collection :as :spacer_template}, &proc" hash)
(puthash 'test_one_arg "string" hash)

(defvar sa-overlay nil)

(defvar show-args-mode nil
  "Dummy variable to suppress compiler warnings.")

(defvar show-args-mode-hook nil
  "Hook for `show-args-mode'.")

(defvar sa-point-at-know-function nil)

(defface sa-face
  '((t (:foreground "darkgray" :underline t)))
  "Face for show args"
  :group 'show-args)

(define-minor-mode show-args-mode
  "Show Argument mode"
  :lighter " SA"
  :group 'show-args
  (if show-args-mode
      (sa-turn-on-mode)
    (sa-turn-off-mode)))

(defun sa-turn-off-mode()
  (remove-hook 'pre-command-hook 'sa-cleanup-if-not-self-insert t)
  (remove-hook 'after-change-functions 'sa-abort-if-not-space-or-open-paren t)
  (remove-hook 'post-command-hook 'sa-show-args-if-function t))

(defun sa-turn-on-mode()
  (add-hook 'after-change-functions 'sa-abort-if-not-space-or-open-paren nil t)
  (add-hook 'post-command-hook 'sa-show-args-if-function nil t)
  (add-hook 'pre-command-hook 'sa-cleanup-if-not-self-insert nil t)
  (run-hooks 'show-args-mode-hook))

(defun sa-cleanup-if-not-self-insert()
  "This is run when an overlay is shown and you do a command that is not inserting chars"
  (if (and (overlayp sa-overlay)
           (not (sa-is-self-insert-command)))
      (sa-cleanup)))

(defun sa-show-args-if-function()
  (if (and (sa-is-self-insert-command)
           (sa-is-word-at-point)
           (sa-is-known-function-at-point))
      (sa-create-functions-overlay-at-point)))

(defun sa-is-self-insert-command()
  (eq this-command 'self-insert-command))

(defun sa-is-word-at-point()
  (> (length (thing-at-point 'symbol)) 0))
  
(defun sa-for-at-point ()
  "get args for function at point"
  (gethash (intern (thing-at-point 'symbol)) hash))

(defun sa-is-known-function-at-point()
  (not (eq nil (sa-for-at-point))))

(defun sa-create-two-spaces-at-point ()
  (insert "  ")
  (backward-char 2))

(defun sa-create-overlay-at-point ()
  (setq sa-overlay (make-overlay (+ 1 (point)) (+ 2 (point))))
  (overlay-put sa-overlay 'font-lock-face 'sa-face)
  (overlay-put sa-overlay 'display (sa-for-at-point))
  (overlay-put sa-overlay 'insert-in-front-hooks '(sa-overlay-insert-key-hook)))
  
(defun sa-create-functions-overlay-at-point ()
  (sa-cleanup)
  (sa-create-two-spaces-at-point)
  (setq sa-point-at-know-function t)
  (sa-create-overlay-at-point))

(defun sa-overlay-insert-key-hook(overlay after begin end &optional length-replaced)
  "This hook is called when you type in front of the args overlay"
  (if after
      (sa-overlay-key-hit overlay begin end)))

(defun sa-overlay-key-hit(overlay begin end)
  (if (sa-did-replace-space-or-comma overlay begin end)
      (sa-delete-first-char-in-overlay overlay)
    (sa-remove-part-of-overlay overlay begin end)))

  
(defun sa-remove-part-of-overlay(overlay begin end)
  (sa-move-overlay-foward-one overlay)
  (sa-remove-up-to-first-comma overlay)
  (if (sa-overlay-empty overlay)
      (sa-cleanup)
    (sa-prepend-comma overlay)))

(defun sa-prepend-comma(overlay) 
  (overlay-put overlay 'display (concat ", " (overlay-get overlay 'display))))

(defun sa-overlay-empty(overlay)
  (eq 0 (length (overlay-get overlay 'display))))

(defun sa-delete-first-char-in-overlay(overlay)
  (sa-remove-first-char-in-overlay overlay)
  (sa-move-overlay-foward-one overlay))

(defun sa-move-overlay-foward-one(overlay)
  (move-overlay overlay (+ 1 (overlay-start overlay)) (+ 2 (overlay-start overlay))))

(defun sa-did-replace-space-or-comma(overlay begin end)
  (or 
   (and (string= "," (buffer-substring-no-properties begin end)) (string= "," (sa-first-char-in-overlay overlay)))
   (and (string= " " (buffer-substring-no-properties begin end)) (string= " " (sa-first-char-in-overlay overlay)))))

(defun sa-first-char-in-overlay(overlay)
  (substring (overlay-get overlay 'display) 0 1))

(defun sa-remove-first-char-in-overlay(overlay)
  (overlay-put overlay 'display (substring (overlay-get overlay 'display) 1)))

(defun sa-remove-up-to-first-comma(overlay)
  (overlay-put overlay 'display (mapconcat 'identity (cdr (split-string (overlay-get overlay 'display) ", ")) ", ")))

(defun sa-abort-if-not-space-or-open-paren(begin end length) 
  (if sa-point-at-know-function
      (if (not (or 
                (string= " " (buffer-substring-no-properties begin end))
                (string= "(" (buffer-substring-no-properties begin end))))
          (sa-cleanup)
        (delete-char 1)
        (setq sa-point-at-know-function nil))))

(defun sa-cleanup()
  (if (overlayp sa-overlay)
      (sa-cleanup-overlay)))

(defun sa-cleanup-overlay()
  (delete-overlay sa-overlay)
  (setq sa-overlay nil)
  (delete-char 1))

(provide 'show-args)
