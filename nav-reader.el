;;; nav-reader.el: Nav reader mode
;;
;; Author: matthew.ozor@gmail.com (Matthew Ozor)
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


(defvar nav-reader-pre-open-list nil
  "list of open buffers not to auto-close")


(defun nav-reader-make-mode-map ()
  "Creates and returns a mode map with bufs's key bindings."
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap "R" 'nav-toggle-reader)
    (define-key keymap "q" 'nav-bufs-quit)
    (define-key keymap "w" 'nav-shrink-wrap)
    (define-key keymap "W" 'nav-set-width-to-default)
    (define-key keymap "?" 'nav-reader-help-screen)
    (define-key keymap [(down)] 'nav-reader-down)
    (define-key keymap [(up)] 'nav-reader-up)
    (define-key keymap [(control ?n)] 'nav-reader-down)
    (define-key keymap [(control ?p)] 'nav-reader-up)
    keymap))

(setq nav-reader-mode-map (nav-reader-make-mode-map))


(defun nav-reader-down ()
  (interactive)
  (next-line)
  (nav-reader-show))


(defun nav-reader-up ()
  (interactive)
  (previous-line)
  (nav-reader-show))
  
  
(defun nav-reader-show ()
  (if (not (looking-at "^.*/$"))
      (progn
	(other-window 1)
	(let ((old-buf (buffer-name (current-buffer))))
	  (kill-buffer old-buf)   ;need to test for already open buffers
	  (select-window (nav-get-window nav-buffer-name))
	  (nav-open-file-under-cursor)
	  (setq buffer-read-only t)
	  (select-window (nav-get-window nav-buffer-name))))))
  

(defun nav-reader-help-screen ()
  "Displays the help screen outside the Nav window."
  (interactive)
  (other-window 1)
  (get-buffer-create "nav-help")
  (switch-to-buffer "nav-help")
  (let ((map (make-sparse-keymap)))
    (use-local-map map)
    (define-key map [mouse-1] 'nav-screen-kill)
    (define-key map [mouse-3] 'nav-screen-kill) 
    (define-key map [mouse-2] 'nav-screen-kill) 
    (define-key map "q" 'nav-screen-kill))
  (setq display-hourglass nil
        buffer-undo-list t)  
  (insert "\
Help for Nav Reader mode
========================

Key Bindings
============

Enter/Return: Jump to buffers under cursor.
Space: Press then space then any other letter to jump to
       filename that starts with that letter.

q\t Quit Nav.
R\t Turn off Reader mode.
w\t Shrink-wrap Nav's window to fit the longest filename in the current directory.
W\t Set the window width to its default value.
?\t Show this help screen.


                Press 'q' or click mouse to quit help

")
  (goto-line 1)
  (view-mode -1)
  (toggle-read-only 1))


(define-derived-mode nav-reader-mode fundamental-mode 
  "Nav-buf-mode is displaying and switching buffers."
  (setq mode-name "Nav reader")
  (use-local-map nav-reader-mode-map))


(defun nav-reader ()
  "Start nav reader mode."
  (interactive)
  (setq nav-reader-pre-open-list (mapcar (function buffer-name) (buffer-list)))
  (nav-save-cursor-line)
  (select-window (nav-get-window nav-buffer-name))
  (nav-reader-mode)
  (if (not (looking-at "^.*/$"))
      (progn
	(nav-open-file-under-cursor)
	(setq buffer-read-only t)
	(select-window (nav-get-window nav-buffer-name)))))


(defun nav-reader-stop ()
  "Stops reader mode."
  (interactive)
  (setq nav-reader nil)
  (nav-mode))


(provide 'nav-reader)

;;; nav-reader.el ends here