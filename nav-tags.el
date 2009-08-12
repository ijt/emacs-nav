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
    (define-key keymap "t" 'nav-tags-quit)
    (define-key keymap [S-down-mouse-3] 'nav-tags-quit)
    (define-key keymap [(tab)] 'forward-button)
    (define-key keymap [(shift tab)] 'backward-button)
    (define-key keymap [(down)] 'forward-button)
    (define-key keymap [(up)] 'backward-button)
    (define-key keymap [(control ?n)] 'forward-button)
    (define-key keymap [(control ?p)] 'backward-button)
    keymap))

(setq nav-tags-mode-map (nav-tags-make-mode-map))

;; Copied imenu internal functions to handle sort.
(defun imenu--sort-by-name (item1 item2)
  (string-lessp (car item1) (car item2)))


(defun nav-tags-fetch-imenu (filename)
  "Generates the tag index from selected file."
  (require 'imenu)
  (setq nav-tags-filename filename)
  (nav-open-file filename)
  (setq index-alist (imenu--make-index-alist t))
  (if imenu-sort-function
      (setq index-alist (sort index-alist imenu-sort-function)))
  (set-buffer nav-buffer-name)
  (select-window (nav-get-window nav-buffer-name))
  (nav-tags))


(defun nav-jump-to-tag-of-button (button)
  ;; This first select-window seems not to do anything. Can we remove it?
  (select-window (nav-get-window nav-buffer-name))

  (let* ((num (replace-regexp-in-string "^.* \\[" "" 
					(button-label button)))
	 (num (substring num 0 -1)))
    (select-window (nav-get-window nav-tags-filename))
    (goto-char (string-to-number num)))

  ;; recenter-top-bottom is not defined in emacs 22.
  (when (functionp 'recenter-top-bottom)
      (recenter-top-bottom)))


(defun nav-tags-show-tags ()
  "Displays all functions in selected file."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (nav-make-header nav-tags-filename))
    (insert "\n")
    (dolist (tag (cdddr index-alist))
      (let* ((tag-name (car tag))
	     (tag-marker (cdr tag))
	     (tag-position (marker-position tag-marker))
	     (button-text (concat tag-name " [" (number-to-string tag-position) "]")))
	(insert-button button-text
		       'action 'nav-jump-to-tag-of-button
		       'follow-link t
		       'face nav-button-face
		       'help-echo nil)
	(insert "\n")))
    (setq mode-line-format "nav: Tag list")
    (force-mode-line-update)
    (setq truncate-lines t)
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