(defvar sa-completions-hash (make-hash-table))

(defun sa-rails-completions()
  (puthash 'redirect_to "Hash | Record | String | Proc | :back, {:status :flash :notice :alert}" sa-completions-hash)
  (puthash 'render "{:action :layout :partial :template :file :text :xml :json :inline :js :nothing :update}, {:locals :status :object :collection :as :spacer_template}, &proc" sa-completions-hash)

  (puthash 'image_tag "source, {:alt :size'wxh' :mouseover html_options}" sa-completions-hash)

  (puthash 'form_for "record, {:url :namespace :html :method :format :as :remote}, &proc" sa-completions-hash)
  (puthash 'check_box "object_name(not needed when called on form builder), method(should return integer), {html_options}, checked_value='1', unchecked_value='0'" sa-completions-hash)
  (puthash 'email_field "object_name(not needed when called on form builder), method, {html_options}" sa-completions-hash)
  (puthash 'fields_for "record_name, record_object = nil, options = {}, &proc" sa-completions-hash)
  (puthash 'file_field "object_name(not needed when called on form builder), method, {html_options}" sa-completions-hash)
  (puthash 'hidden_field "object_name(not needed when called on form builder), method, {html_options}" sa-completions-hash)
  (puthash 'label "object_name(not needed when called on form builder), method, content_or_options=nil, {html_options}, &proc" sa-completions-hash)
  (puthash 'number_field "object_name(not needed when called on form builder), method, {html_options}" sa-completions-hash)
  (puthash 'password_field "object_name(not needed when called on form builder), method, {html_options}" sa-completions-hash)
  (puthash 'phone_field "object_name(not needed when called on form builder), method, {html_options}" sa-completions-hash)
  (puthash 'radio_button "object_name(not needed when called on form builder), method, tag_value(string to set checked to), {html_options}" sa-completions-hash)
  (puthash 'range_field "object_name(not needed when called on form builder), method, {html_options}" sa-completions-hash)
  (puthash 'search_field "object_name(not needed when called on form builder), method, {html_options}" sa-completions-hash)
  (puthash 'telephone_field "object_name(not needed when called on form builder), method, {html_options}" sa-completions-hash)
  (puthash 'text_area "object_name(not needed when called on form builder), method, {html_options}" sa-completions-hash)
  (puthash 'text_field "object_name(not needed when called on form builder), method, {html_options}" sa-completions-hash)
  (puthash 'url_field "object_name(not needed when called on form builder), method, {html_options}" sa-completions-hash)

  (puthash 'button_tag "content_or_options, {:confirm :disabled :disabled_with html_options}, &proc" sa-completions-hash)
  (puthash 'check_box_tag "name, value='1', checked=false, {:disabled html_options}" sa-completions-hash)
  (puthash 'email_tag "name, value=nil, {html_options}" sa-completions-hash)
  (puthash 'field_set_tag "legend=nil, {html_options}, &proc" sa-completions-hash)
  (puthash 'file_field_tag "name, {:disabled html_options}" sa-completions-hash)
  (puthash 'file_field_tag "name, {:disabled html_options}" sa-completions-hash)
  (puthash 'form_tag "url_or_options, {:multipart :method :authenticity_token :remote html_options}, &proc" sa-completions-hash)
  (puthash 'hidden_field_tag "name, value=nil, {html_options}, &proc" sa-completions-hash)
  (puthash 'image_submit_tag "source, {:confirm :disabled html_options}" sa-completions-hash)
  (puthash 'label_tag "name, value, {html_options}, &proc" sa-completions-hash)
  (puthash 'number_field_tag "name, value=nil, {:min :max :in :step :disabled :size :maxlength :placeholder html_options}" sa-completions-hash)
  (puthash 'password_field_tag "name, value=nil, {:disabled :size :maxlength html_options}" sa-completions-hash)
  (puthash 'phone_field_tag "name, value=nil, {:disabled :size :maxlength :placeholder html_options}" sa-completions-hash)
  (puthash 'radio_button_tag "name, value, checked=false, {:disabled html_options}" sa-completions-hash)
  (puthash 'range_field_tag "name, value, {:min :max :in :step :disabled :size :maxlength :placeholder html_options}" sa-completions-hash)
  (puthash 'search_field_tag "name, value, {:disabled :size :maxlength :placeholder html_options}" sa-completions-hash)
  (puthash 'select_tag "name, {option_tags}, {:multiple :disabled :include_blank :prompt html_options}" sa-completions-hash)
  (puthash 'submit_tag "value='Save Changes', {:confirm :disabled :disable_with html_options}" sa-completions-hash)
  (puthash 'telephone_field_tag "name, value=nil, {:disabled :size :maxlength :placeholder html_options}" sa-completions-hash)
  (puthash 'text_area_tag "name, content=nil, {:size :rows :cols :disabled :escape html_options}" sa-completions-hash)
  (puthash 'text_field_tag "name, value=nil, {:disabled :size :maxlength :placeholder html_options}" sa-completions-hash)
  (puthash 'url_field_tag "name, value=nil, {:disabled :size :maxlength :placeholder html_options}" sa-completions-hash)



  (puthash 'test_one_arg "string" sa-completions-hash))

(defvar sa-overlay nil)

(defvar show-args-mode nil
  "Dummy variable to suppress compiler warnings.")

(defvar show-args-mode-hook nil
  "Hook for `show-args-mode'.")

(defvar sa-overlay-line-number nil
  "The line the show args overlay is on, used to determine when to clear the overlay")

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
  (remove-hook 'pre-command-hook 'sa-set-before-command-point-and-line t)
  (remove-hook 'after-change-functions 'sa-abort-if-not-space-or-open-paren t)
  (remove-hook 'post-command-hook 'sa-show-args-if-function t))

(defun sa-turn-on-mode()
  (make-variable-buffer-local 'sa-point-at-know-function)
  (make-variable-buffer-local 'sa-overlay-line-number)
  (make-variable-buffer-local 'sa-overlay)
  (make-variable-buffer-local 'sa-completions-hash)
  (sa-rails-completions)
  (add-hook 'after-change-functions 'sa-abort-if-not-space-or-open-paren nil t)
  (add-hook 'post-command-hook 'sa-show-args-if-function-clear-old-overlays nil t)
  (add-hook 'pre-command-hook 'sa-set-before-command-point-and-line nil t)
  (run-hooks 'show-args-mode-hook))

(defun sa-set-before-command-point-and-line()
  (setq sa-overlay-line-number (line-number-at-pos)))

(defun sa-show-args-if-function-clear-old-overlays()
  (if (and (overlayp sa-overlay)
           (not (eq sa-overlay-line-number (line-number-at-pos))))
      (sa-cleanup))

  (if (and (sa-is-self-insert-command)
           (sa-is-word-at-point)
           (sa-is-known-function-behind-point))
      (sa-create-functions-overlay-at-point)))

(defun sa-is-self-insert-command()
  (eq this-command 'self-insert-command))

(defun sa-is-word-at-point()
  (> (length (thing-at-point 'symbol)) 0))
  
(defun sa-for-at-point ()
  "get args for function at point"
  (gethash (intern (thing-at-point 'symbol)) sa-completions-hash))

(defun sa-is-known-function-behind-point()
  (and (eq (point) (cdr (bounds-of-thing-at-point 'symbol)))
       (not (eq nil (sa-for-at-point)))))

(defun sa-create-two-spaces-at-point ()
  (insert "  ")
  (backward-char 2)
  (put-text-property (point) (+ 2 (point)) 'sa-extra-space t))

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
      (let ((inhibit-modification-hooks t))
        (sa-overlay-key-hit overlay begin end))))

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
  
  (save-excursion
    (while (text-property-any (point-min) (point-max) 'sa-extra-space t)
      (goto-char (text-property-any (point-min) (point-max) 'sa-extra-space t))
      (delete-char 1))))

(provide 'show-args)
