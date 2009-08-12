;; nav-tags.el

;; imenu returns a list in this format
;; (("rescan" . -99) ("Types" (0 . 0) (0 . 0)) ("Variables" (0 . 0) (0 . 0))
;;  (fun1 . marker) (fun2 . marker) (fun3 . marker))

;; Make sure we can get tags for Python.
(condition-case err
    (require 'python)
  (error
   'error-setting-up-python-support))

(defvar nav-tags-alist nil
  "Association list from tag names to positions")


(defun nav-tags-make-mode-map ()
  "Creates and returns a mode map with tags's key bindings."
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap "w" 'nav-shrink-wrap)
    (define-key keymap "W" 'nav-set-width-to-default)
    (define-key keymap "q" 'nav-quit)
    (define-key keymap "t" 'nav-tags-quit)
    (define-key keymap "T" 'nav-tags-sort)
    (define-key keymap "?" 'nav-help-screen)
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


(defun nav-marker-to-position (maybe-marker)
  "Converts a marker to a position, or just returns the arg unchanged."
  (if (markerp maybe-marker)
      (marker-position maybe-marker)
    maybe-marker))


(defun nav-tags-flatten (name-and-info)
  "Converts class tags into flat names of class methods."
  (let ((name (car name-and-info))
	(info (cdr name-and-info)))
    (if (and (string-match "^class [a-zA-Z0-9_]+$" name)
	     (listp info))
	(let ((class-pos (cdr (car info)))
	      (class-name (substring name (length "class ") (length name))))
	  (cons (cons name class-pos)
		(mapcar (lambda (method-name-and-pos)
			  (let ((method-name (car method-name-and-pos))
				(pos (cdr method-name-and-pos)))
			    (cons (concat class-name "." method-name)
				  pos)))
			(cddr name-and-info))))
      (list name-and-info))))


(defun nav-marker-to-pos-in-pair (name-and-maybe-marker)
  (let ((name (car name-and-maybe-marker))
	(maybe-marker (cdr name-and-maybe-marker)))
    (cons name (nav-marker-to-position maybe-marker))))


(defun nav-make-tags-alist ()
  "Builds the tags association list from the current buffer."
  (let* ((alist (imenu--make-index-alist t))
	 ;; Maybe sort.
	 (alist (if imenu-sort-function
		    (sort alist imenu-sort-function)
		  alist))
	 (alists (mapcar 'nav-tags-flatten alist))
	 (alist (apply 'append alists))
	 )
    alist))


(defun nav-tags-fetch-imenu (filename)
  "Generates the tag index from selected file."
  (require 'imenu)
  (setq nav-tags-filename filename)
  (nav-open-file filename)
  (setq nav-tags-alist (nav-make-tags-alist))
  (set-buffer nav-buffer-name)  ; Can we remove this?
  (select-window (nav-get-window nav-buffer-name))
  (nav-tags))


(defun nav-jump-to-tag-of-button (button)
  ;; For sorting?
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
    (dolist (tag (cddr nav-tags-alist))
      (let* ((tag-name (car tag))
	     (tag-position (nav-marker-to-position (cdr tag))))
	(let ((button-text (concat tag-name " [" (format "%s" tag-position) "]")))
	    (insert-button button-text
			   'action 'nav-jump-to-tag-of-button
			   'follow-link t
			   'face nav-button-face
			   'help-echo nil)
	    (insert "\n"))))
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