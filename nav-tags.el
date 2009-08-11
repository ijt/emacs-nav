;; nav-tags.el

;; imenu returns a list in this format
;; (("rescan" . -99) ("Types" (0 . 0) (0 . 0)) ("Variables" (0 . 0) (0 . 0))
;;  (fun1 . marker) (fun2 . marker) (fun3 . marker))

(defun nav-tags-make-mode-map ()
  "Creates and returns a mode map with tags's key bindings."
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap "T" 'nav-tags-sort)
    (define-key keymap "w" 'nav-shrink-wrap)
    (define-key keymap "W" 'nav-set-width-to-default)
    (define-key keymap "q" 'nav-quit)
    (define-key keymap "?" 'nav-help-screen)
    (define-key keymap "`" 'nav-tags-quit)
    (define-key keymap [S-down-mouse-3] 'nav-tags-quit)
    (define-key keymap [(tab)] 'forward-button)
    (define-key keymap [(shift tab)] 'backward-button)
    (define-key keymap [(down)] 'forward-button)
    (define-key keymap [(up)] 'backward-button)
    (define-key keymap [(control ?n)] 'forward-button)
    (define-key keymap [(control ?p)] 'backward-button)
    keymap))

(setq nav-tags-mode-map (nav-tags-make-mode-map))

;; copied imenu internal functions to handle sort.
(defun imenu--sort-by-name (item1 item2)
  (string-lessp (car item1) (car item2)))


(defun nav-tags-fetch-imenu (file)
  "Generates the tag index from selected file."
  (require 'imenu)
  (setq nav-tags-filename file)
  (nav-open-file file)
  (setq imenu--index-alist nil)
  (setq index-alist (imenu--make-index-alist t))
  (if imenu-sort-function
      (setq index-alist (sort index-alist imenu-sort-function)))
  (set-buffer nav-buffer-name)
  (select-window (nav-get-window nav-buffer-name))
  (nav-tags))


(defun nav-tags-show-tags ()
  "Displays all functions in selected file."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (nav-make-header nav-tags-filename))
    (insert "\n")
    (setq nav-vars (cdr (cdr (cdr index-alist))))
    (while nav-vars
      (setq nav-tag-fun (car nav-vars))
      (setq nav-tag-text (car nav-tag-fun))
      (setq nav-tag-marker (cdr nav-tag-fun))
      (setq nav-tag-buffer (marker-buffer nav-tag-marker))
      (setq nav-tag-position (marker-position nav-tag-marker))
      ;make button here
      (setq nav-tag-button-text (concat nav-tag-text " [" (number-to-string nav-tag-position) "]"))
      (insert-button nav-tag-button-text
		     'action (lambda (button)
			       (select-window (nav-get-window nav-buffer-name))
			       (setq num (replace-regexp-in-string "^.* \\[" "" 
								   (button-label button)))
			       (setq num (substring num 0 -1))
			       (select-window (nav-get-window nav-tags-filename))
			       (goto-char (string-to-number num))
			       (recenter-top-bottom))
		     'follow-link t
		     'face nav-button-face
		     'help-echo nil)
      (insert "\n")
      (setq nav-vars (cdr nav-vars)))
    (setq mode-line-format "nav: Tag list")
    (force-mode-line-update)
    (goto-line 2)))


(defun nav-tags-sort ()
  "Toggles sort to by name/position and re-displays tags"
  (interactive)
  (if (eq imenu-sort-function 'imenu--sort-by-name)
      (setq imenu-sort-function nil)
    (setq imenu-sort-function 'imenu--sort-by-name))
  (nav-tags-fetch-imenu nav-tags-filename))


(defun nav-tags-quit ()
  "Kill nav-tags."
  (interactive)
  (nav-mode))
  

(define-derived-mode nav-tags-mode fundamental-mode 
  "Nav-buf-mode is displaying and switching buffers."
  (setq mode-name "Nav tags")
  (use-local-map nav-tags-mode-map)
  (setq buffer-read-only t)
  (nav-tags-show-tags))


(defun nav-tags ()
  "Run nav-tags-mode on top of nav."
  (interactive)
  (nav-tags-mode))


(provide 'nav-tags)

;;; nav-tags.el ends here