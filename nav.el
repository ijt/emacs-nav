;;; nav.el --- Emacs mode for filesystem navigation
;;
;; Copyright 2010 Google Inc. All Rights Reserved.
;;
;; Author: issactrotts@google.com (Issac Trotts)
;; Version: 20101227
;;

;;; License:
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;      http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;;; Commentary:
;; 
;; To use this file, put something like the following in your
;; ~/.emacs:
;;
;; (add-to-list 'load-path "/directory/containing/nav/")
;; (require 'nav)
;;
;; Type M-x nav to start navigating.
;;

;;; Key Bindings
;;
;; Press ? in the Nav window to display a list of key bindings.
;;

;;; History:
;;
;; See http://code.google.com/p/emacs-nav/source/list
;;

;;; Code:

(condition-case err
    (require 'ack)
  (error
   (message "Could not load ack.")))

(defgroup nav nil
  "A lightweight filesystem navigator."
  :group 'applications)

(defcustom nav-filtered-p t
  "*If true, nav will filter out files and directories such as
hidden files, backups and .elc files.  The result depends on
`nav-boring-file-regexps'.
"
  :type 'boolean
  :group 'nav)

(defcustom nav-boring-file-regexps
  (list "^[.][^.].*$"        ; hidden files such as .foo
	"^[.]$"              ; current directory
	"~$"
	"[.]elc$"
	"[.]pyc$"
	"[.]o$"
	"[.]bak$"
	;; Stolen from Ack:
	"^_MTN$"  ; Monotone
	"^blib$"  ; Perl module building
	"^CVS$"  ; CVS
	"^RCS$"  ; RCS
	"^SCCS$"  ; SCCS
	"^_darcs$"  ; darcs
	"^_sgbak$"  ; Vault/Fortress
	"^autom4te.cache$"  ; autoconf
	"^cover_db$"  ; Devel::Cover
	"^_build$"  ; Module::Build
	)
  "*Nav ignores filenames that match any regular expression in this list."
  :type '(repeat string)
  :group 'nav)


;;
;; Fontification
;;
(require 'dired)

(defvar nav-directory-face 'dired-directory
  "Face name used for directories.")

(defvar nav-font-lock-keywords
 '(("^.*/$" . nav-directory-face))
 "Regexes and associated faces used by Nav to fontify files and
directories."
)


(defcustom nav-widths-percentile 100
  "*What percentage of files should remain completely visible when shrink-wrapping."
  :type 'integer
  :group 'nav)

(defun nav-make-mode-map ()
  "Creates and returns a mode map with nav's key bindings."
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap "\n" 'nav-open-file-under-cursor-other-window)
    (define-key keymap "\r" 'nav-open-file-under-cursor-other-window)
    (define-key keymap "a" 'ack)
    (define-key keymap "c" 'nav-copy-file-or-dir)
    (define-key keymap "C" 'nav-customize)
    (define-key keymap "d" 'nav-delete-file-or-dir-on-this-line)
    (define-key keymap "e" 'nav-invoke-dired)  
    (define-key keymap "f" 'nav-find-files)
    (define-key keymap "g" 'grep-find)
    (define-key keymap "h" 'nav-jump-to-home)
    (define-key keymap "j" 'nav-jump-to-dir)
    (define-key keymap "m" 'nav-move-file-or-dir)
    (define-key keymap "n" 'nav-make-new-directory)
    (define-key keymap "o" 'nav-open-file-under-cursor)
    (define-key keymap "p" 'nav-pop-dir)
    (define-key keymap "P" 'nav-print-current-dir)
    (define-key keymap "q" 'nav-quit)
    (define-key keymap "r" 'nav-refresh)
    (define-key keymap "s" 'nav-shell)
    (define-key keymap "u" 'nav-go-up-one-dir)
    (define-key keymap "w" 'nav-shrink-wrap)
    (define-key keymap "!" 'nav-shell-command)
    (define-key keymap "." 'nav-toggle-hidden-files)
    (define-key keymap "?" 'nav-help-screen)
    (define-key keymap " " 'nav-jump-to-name)

    ;; Avoid [(tab)] and [(shift tab)] because they don't always work.
    (define-key keymap "\t" 'forward-button)
    (define-key keymap [backtab] 'backward-button)

    (define-key keymap [(down)] 'forward-button)
    (define-key keymap [(up)] 'backward-button)
    (define-key keymap [(control ?n)] 'forward-button)
    (define-key keymap [(control ?p)] 'backward-button)
    keymap))

(defun nav-quit-help ()
  "Exits the nav help screen."
  (interactive)
  (kill-buffer (current-buffer)))

(defun nav-help-screen ()
  "Displays the help screen."
  (interactive)
  (switch-to-buffer "*nav-help*")
  (let ((map (make-sparse-keymap)))
    (use-local-map map)
    (define-key map "q" 'nav-quit-help))
  (setq display-hourglass nil
        buffer-undo-list t)
  (insert "\
Nav Key Bindings
================

Enter/Return: Go to directory under cursor, or open file under
              cursor in other window.

Tab: Move forward through filenames.
Shift-Tab: Move backward through filenames.

Space: Press spacebar, then any other letter to jump to filename
       that starts with that letter.

a\t Recursively grep for a Perl regex using Ack (http://betterthangrep.com/).
c\t Copy file or directory under cursor.
C\t Customize Nav settings and bookmarks.
d\t Delete file or directory under cursor (asks to confirm first).
e\t Edit current directory in dired.
f\t Recursively find files whose titles match a Perl regex (using Ack).
g\t Grep recursively from current directory using grep-find
h\t Jump to home (~).
j\t Jump to another directory.
m\t Move or rename file or directory.
n\t Make a new directory.
o\t Open file under cursor in the nav window.
p\t Pop directory stack to go back to the previous directory.
P\t Print full path of current displayed directory.
q\t Quit nav.
r\t Refresh.
s\t Start a shell in an emacs window in the current directory.
u\t Go up to parent directory.
w\t Shrink-wrap Nav's window to fit the longest filename in the current directory.
 \t Hit C-x + to roughly undo this by balancing windows.
!\t Run shell command.
.\t Toggle display of hidden files.
?\t Show this help screen.

                Press 'q' to quit help

")
  (nav-goto-line 1)
  (view-mode -1)
  (toggle-read-only 1))

(defvar nav-mode-map
  (nav-make-mode-map)
  "Mode map for nav mode")

(defvar nav-dir-stack '())

(defvar nav-map-dir-to-line-number (make-hash-table :test 'equal)
  "Hash table from dir paths to most recent cursor pos in them.")

(defvar nav-button-face nil)

(defconst nav-default-line-num 2
  "Line where cursor starts in directories that have not yet been
visited. A value of 1 would start the cursor off on ../.")

(defconst nav-shell-buffer-name "*nav-shell*"
  "Name of the buffer used for the command line shell spawned by
  nav on the 's' key.")

(defconst nav-buffer-name "*nav*"
  "Name of the buffer where nav shows directory contents.")

(defconst nav-buffer-name-for-find-results "*nav-find*"
  "Name of the buffer where nav shows find results.")

(defun nav-join (sep string-list)
  (mapconcat 'identity string-list sep))

(defun nav-toggle-hidden-files ()
  (interactive) 
  (setq nav-filtered-p (not nav-filtered-p))
  (nav-refresh))

(defun nav-filename-matches-some-regexp (filename regexps)
  (let ((matches-p nil))
    (dolist (rx regexps)
      (if (string-match rx filename)
          (setq matches-p t)))
      matches-p))

;; http://www.emacswiki.org/emacs/ElispCookbook#toc41
(defun nav-filter (condp lst)
  (delq nil
	(mapcar (lambda (x) (and (funcall condp x) x)) lst)))

(defun nav-filter-out-boring-filenames (filenames boring-regexps)
  (nav-filter 
   (lambda (filename)
     (not (nav-filename-matches-some-regexp filename boring-regexps)))
   filenames))

(defun nav-get-line-for-cur-dir ()
  (gethash (nav-get-working-dir) nav-map-dir-to-line-number))

(defun nav-cd (dirname)
  "Changes to a different directory and pushes it onto the stack."
  (let ((dirname (file-name-as-directory (file-truename dirname))))
    (nav-save-cursor-line)
    (setq default-directory dirname)
    (nav-show-dir dirname)
    (nav-restore-cursor-line)))

(defun nav-save-cursor-line ()
  "Updates line number hash table."
  (let ((line-num (nav-line-number-at-pos (point))))
    (puthash (nav-get-working-dir) line-num nav-map-dir-to-line-number)))

(defun nav-goto-line (line)
  "Jumps point to the given line."
  (goto-char (point-min)) (forward-line (1- line)))

(defun nav-restore-cursor-line ()
  "Remembers what line we were on last time we visited this directory."
  (let ((line-num (or (nav-get-line-for-cur-dir)
		      nav-default-line-num)))
    (nav-goto-line line-num)))

(defun nav-open-file (filename)
  "Opens a file or directory from Nav."
  (interactive "FFilename:")
  (if (file-directory-p filename)
      (nav-push-dir filename)
    (find-file filename)))

(defun nav-open-file-other-window (filename)
  "Opens a file or directory from Nav."
  (interactive "FFilename:")
  (if (file-directory-p filename)
      (nav-push-dir filename)
    (find-file-other-window filename)))

(defun nav-open-file-under-cursor ()
  "Finds the file under the cursor."
  (interactive)
  (let ((filename (nav-get-cur-line-str)))
    (nav-open-file filename)))

(defun nav-open-file-under-cursor-other-window ()
  "Finds the file under the cursor, in another window."
  (interactive)
  (let ((filename (nav-get-cur-line-str)))
    (find-file-other-window filename)))

(defun nav-go-up-one-dir ()
  "Points Nav to ../."
  (interactive)
  (nav-push-dir ".."))

(defun nav-shrink-wrap ()
  "Updates the width of the Nav window to fit the longest filename in the
current directory. Updates the global variable nav-width as a side effect."
  (interactive)
  (let* ((lines (split-string (buffer-string) "\n" t))
	 (num-lines (length lines))
	 (line-lengths (mapcar 'length lines))
	 (desired-width (+ 1 (nav-percentile nav-widths-percentile
					     (sort line-lengths '<))))
	 (max-width (/ (frame-width) 2))
	 (new-width (min desired-width max-width)))
    (nav-set-window-width new-width)))

(defun nav-percentile (percent sorted-things)
  "Returns the item a certain percent of the way through a list of items
assumed to be sorted."
  (let* ((n (length sorted-things))
	 (k (min (- n 1 )
		 (truncate (* (/ percent 100.0) n)))))
    (nth k sorted-things)))

(defun nav-push-dir (dirname)
  (let ((dirname (file-truename dirname)))
    (when (not (string= dirname default-directory))
      (push (file-truename default-directory) nav-dir-stack)
      (nav-cd dirname))))

(defun nav-pop-dir ()
  "Goes to the previous directory in Nav's history.
This works like a web browser's back button."
  (interactive)
  (let ((dir nil))
    (while (and nav-dir-stack
                (or (not dir)
                    (equal dir (file-name-as-directory (file-truename ".")))
                    (not (file-exists-p dir))))
      (setq dir (pop nav-dir-stack)))
    (setq dir (or dir "."))
    (nav-cd dir)))

(defun nav-get-cur-line-str ()
  (buffer-substring-no-properties (point-at-bol)
                                  (point-at-eol)))

(defun nav-non-boring-directory-files (dir)
  (nav-filter-out-boring-filenames (directory-files dir)
				   (if nav-filtered-p
				       nav-boring-file-regexps
				     '()
				     )))

(defun nav-dir-suffix (dir)
  (replace-regexp-in-string ".*/" "" (directory-file-name dir)))

(defun nav-line-number-at-pos (p)
  (let ((line-num 1))
    (dotimes (i p line-num)
      (if (eq ?\n (char-after i))
          (setq line-num (+ line-num 1))))))

(defun nav-replace-buffer-contents (new-contents)
  (let ((saved-line-number (nav-line-number-at-pos (point)))
        ;; Setting inhibit-read-only to t here lets us edit the buffer
        ;; in this let-block.
        (inhibit-read-only t))
    (erase-buffer)
    (insert new-contents)
    (nav-make-filenames-clickable)
    (nav-goto-line saved-line-number)))

(defun nav-button-action-to-open-file (button)
  (let ((buffer (overlay-buffer button)))
    (pop-to-buffer buffer)
    (if (= 1 (count-windows))
	(split-window-horizontally))
    (nav-open-file-other-window (button-label button))))

(defun nav-button-action-to-open-dir (button)
  (let ((buffer (overlay-buffer button)))
    (pop-to-buffer buffer)
    (nav-push-dir (button-label button))))

(defun nav-make-filenames-clickable ()
  (condition-case err
      (save-excursion
	(nav-goto-line 1)
	(dotimes (i (count-lines 1 (point-max)))
	  (let* ((start (line-beginning-position))
		 (end (line-end-position))
		 (filename (buffer-substring-no-properties start end))
		 (action (if (file-directory-p filename)
			     'nav-button-action-to-open-dir
			   'nav-button-action-to-open-file)))
	    (make-button start end
			 'action action
			 'follow-link t
			 'face nav-button-face
			 'help-echo nil))
	  (forward-line 1)))
    (error 
     ;; This can happen for versions of emacs that don't have
     ;; make-button defined.
     'failed)))

(defun nav-string< (s1 s2)
  "Tells whether S1 comes lexically before S2, ignoring case."
  (string< (downcase s1) (downcase s2)))

(defun nav-show-dir (dir)
  (let ((new-contents '()))
    (dolist (filename (nav-non-boring-directory-files dir))
      (let ((line (concat filename
                          (if (file-directory-p filename)
                              "/"
                            "")
			  )))
        (push line new-contents)))
    (let* ((new-contents (sort new-contents 'nav-string<))
           (new-contents (nav-join "\n" new-contents)))
      (nav-replace-buffer-contents new-contents))
    (setq mode-line-format (nav-make-mode-line "d" dir))
    (force-mode-line-update)))

(defun nav-set-window-width (n)
  (if (> (window-width) n)
    (shrink-window-horizontally (- (window-width) n)))
  (if (< (window-width) n)
    (enlarge-window-horizontally (- n (window-width)))))

(defun nav-get-working-dir ()
  (file-name-as-directory (file-truename default-directory)))

(defun nav-invoke-dired ()
  "Invokes dired on the current directory."
  (interactive)
  (dired (nav-get-working-dir)))

(defun nav-find-files (pattern)
  "Recursively finds files whose names match a Perl regular expression."
  (interactive "sPattern: ")
  (let* ((pattern (format "%s[^/]*$" pattern))
	 (find-command (format "ack -a -l '.' | ack %s" pattern))
	 (inhibit-read-only t))
    (erase-buffer)
    (call-process-shell-command find-command nil (current-buffer))
    (nav-make-filenames-clickable)
    (message "Hit r to bring back Nav directory listing.")
    (cond ((string= "" (buffer-string))
	   (insert "No matching files found."))
	  (t
	   ;; Enable nav keyboard shortcuts, mainly so hitting enter will open
	   ;; files.
	   (use-local-map nav-mode-map))
	  )
    (forward-line -1)
    ))

(defun nav-refresh ()
  "Resizes Nav window to original size, updates its contents."
  (interactive)
  (nav-show-dir ".")
  (nav-restore-cursor-line))

(defun nav-quit ()
  "Exits Nav."
  (interactive)
  (kill-buffer (current-buffer)))

(defun nav-jump-to-home ()
  "Show home directory in Nav."
  (interactive)
  (nav-push-dir "~"))

(defun nav-jump-to-name (arg)
  (interactive "K")
  (nav-goto-line 2)
  (let ((nav-search-string (concat "^" arg)))
    (search-forward-regexp nav-search-string)))

(defun nav-jump-to-dir (dirname)
  "Shows a specified directory in Nav."
  (interactive "fDirectory: ")
  (nav-push-dir dirname))

(defun nav-make-mode-line (mode dir)
  (concat "-(nav)" 
	  (nav-dir-suffix (file-truename dir))
	  "/"
	  " "
	  (format "[%s]" 
		  (if nav-filtered-p
		      "filtered"
		    "unfiltered"))
	  )
  )

(defun nav-this-is-a-microsoft-os ()
  (or (string= system-type "windows-nt")
      (string= system-type "ms-dos")))

(defun nav-make-remove-dir-command (dirname)
  (if (nav-this-is-a-microsoft-os)
      (format "rmdir /S /Q \"%s\"" dirname)
    (format "rm -rf '%s'" dirname)))

(defun nav-delete-file-or-dir (filename)
  (nav-save-cursor-line)
  (if (and (file-directory-p filename)
           (not (file-symlink-p (directory-file-name filename))))
      (when (yes-or-no-p (format "Really delete directory %s ?" filename))
        (shell-command (nav-make-remove-dir-command filename))
        (nav-refresh))
      ;; We first use directory-file-name to strip the trailing slash
      ;; if it's a symlink to a directory.
      (let ((filename (directory-file-name filename)))
        (when (y-or-n-p (format "Really delete file %s ? " filename))
          (delete-file filename)
          (nav-refresh))))
  (nav-restore-cursor-line))

(defun nav-delete-file-or-dir-on-this-line ()
  "Deletes a file or directory."
  (interactive)
  (nav-delete-file-or-dir (nav-get-cur-line-str)))

(defun nav-ok-to-overwrite (target-name)
  "Returns non-nil if it's ok to overwrite or create a file.

That is, if a file with the given name doesn't exist, is a
directory, or if the user says it's ok."
  (or (not (file-exists-p target-name))
      (file-directory-p target-name)
      (y-or-n-p (format "Really overwrite %s ? " target-name))))

(defun nav-copy-file-or-dir (target-name)
  "Copies a file or directory."
  (interactive "FCopy to: ")
  (let ((filename (nav-get-cur-line-str)))
    (if (nav-this-is-a-microsoft-os)
        (copy-file filename target-name)
      (if (nav-ok-to-overwrite target-name)
          (let ((maybe-dash-r (if (file-directory-p filename) "-r" "")))
            (shell-command (format "cp %s '%s' '%s'" maybe-dash-r
                                   (expand-file-name filename)
                                   (expand-file-name target-name)))))))
  (nav-refresh))

(defun nav-customize ()
  "Starts customization for Nav."
  (interactive) 
  (customize-group "nav"))

(defun nav-move-file-or-dir (target-name)
  "Moves a file or directory."
  (interactive "FMove to: ")
  (let ((filename (nav-get-cur-line-str)))
    (if (nav-this-is-a-microsoft-os)
        (rename-file filename target-name)
      (if (nav-ok-to-overwrite target-name)
          (shell-command (format "mv '%s' '%s'"
                                 (expand-file-name filename)
                                 (expand-file-name target-name))))))
  (nav-refresh))

(defun nav-append-slashes-to-dir-names (names)
  (mapcar (lambda (name)
            (if (file-directory-p name)
                (concat name "/")
              name))
          names))

(defun nav-make-new-directory (name)
  "Creates a new directory."
  (interactive "sMake directory: ")
  (make-directory name)
  (nav-refresh))

(defun nav-shell ()
  "Starts up a shell on the current nav directory.

Thanks to claudio.bley for this new, improved version.
http://code.google.com/p/emacs-nav/issues/detail?id=78
"
  (interactive)
  (let ((default-directory (nav-get-working-dir)))
    (shell nav-shell-buffer-name)))

(defun nav-shell-command (command)
  "Runs a shell command and then refreshes the Nav window."
  (interactive "sShell command: ")
  (shell-command command)
  (nav-refresh))

(defun nav-print-current-dir ()
  "Shows the full path that nav is currently displaying"
  (interactive)
  (print default-directory))

(define-derived-mode nav-mode fundamental-mode 
  "Nav mode navigates filesystems."
  (setq mode-name "Nav")
  (use-local-map nav-mode-map)
  (setq buffer-read-only t)
  (setq truncate-lines t)
  (setq font-lock-defaults '(nav-font-lock-keywords))
  (nav-refresh))

(defun nav-disable-emacs23-window-splitting ()
  "Turns off the new feature where Emacs 23 automatically splits windows when
opening files in a large frame."
  (interactive)
  (setq split-width-threshold most-positive-fixnum)
  (setq split-height-threshold most-positive-fixnum))

;; The next line is for ELPA, the Emacs Lisp Package Archive.
;;;###autoload
(defun nav ()
  (interactive)
  (let ((default-directory (nav-get-working-dir)))
    (switch-to-buffer (generate-new-buffer-name nav-buffer-name))
    (nav-mode)
    (nav-refresh)))

(provide 'nav)

;;; nav.el ends here
