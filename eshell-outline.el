;;; eshell-outline.el --- Enhanced outline-mode for Eshell  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Jamie Beardslee

;; Author: Jamie Beardslee <jdb@jamzattack.xyz>
;; Keywords:
;; Version: 2020.08.06
;; URL: https://git.jamzattack.xyz/eshell-outline

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:
(require 'eshell)
(require 'outline)

;;;###autoload
(defun eshell-outline-view-buffer ()	; temporary
  "Clone the current eshell buffer, and enable `outline-mode'.

This will clone the buffer via `clone-indirect-buffer', so all
following changes to the original buffer will be transferred.

The command `eshell-outline-mode' offers a more interactive
version, with specialized keybindings."
  (interactive)
  (let* ((buffer
	  (clone-indirect-buffer (generate-new-buffer-name "*eshell outline*") nil)))
    (with-current-buffer buffer
      (outline-mode)
      (setq-local outline-regexp eshell-prompt-regexp)
      (outline-hide-body))
    (pop-to-buffer buffer)))


;;; Internal functions

(defun eshell-outline--final-prompt-p ()
  "Return t if a process is running or point is it final prompt."
  (or eshell-process-list
      (save-excursion
	(not (eshell-previous-prompt 1)))))


;;; Commands

(defun eshell-outline-toggle-or-interrupt (&optional int)
  "Interrupt the process or toggle outline children.

With prefix arg INT, or if point is on the final prompt, send an
interrupt signal to the running process.

Otherwise, show or hide the heading (i.e. command) at point."
  (interactive "P")
  (if (or int (eshell-outline--final-prompt-p))
      (eshell-interrupt-process)
    (outline-toggle-children)))

(defun eshell-outline-toggle-or-kill (&optional kill)
  "Kill the process or toggle outline children.

With prefix arg KILL, or if point is on the final prompt, send a
kill signal to the running process.

Otherwise, show or hide the heading (i.e. command) at point.

Note: This does not act like `outline-show-branches', as
`eshell-outline-mode' only goes 1 level deep."
  (interactive "P")
  (if (or kill (eshell-outline--final-prompt-p))
      (eshell-kill-process)
    (outline-show-children)))

(defun eshell-outline-mark ()
  "Mark the current prompt and output.

If point is at the end of the buffer, "
  (interactive)
  (if (= (point) (point-max))
      (forward-line -1))
  (outline-mark-subtree))


;;; The minor mode

(defvar eshell-outline-mode-map (make-sparse-keymap)
  "The keymap for `eshell-outline-mode'.")

(setq
 ;; the `setq' is for development, TODO move to `defvar'
 eshell-outline-mode-map
 (let ((map (make-sparse-keymap)))
   ;; eshell-{previous,next}-prompt are the same as
   ;; outline-{next,previous} -- no need to bind these.

   (define-key map (kbd "C-c C-c") #'eshell-outline-toggle-or-interrupt)
   (define-key map (kbd "C-c C-k") #'eshell-outline-toggle-or-kill)
   (define-key map (kbd "C-c M-m") #'eshell-outline-mark)

   ;; From outline.el
   (define-key map (kbd "C-c C-a") #'outline-show-all)
   (define-key map (kbd "C-c C-t") #'outline-hide-body)

   ;; Default `outline-minor-mode' keybindings
   (define-key map (kbd "C-c @") outline-mode-prefix-map)
   map))

;;;###autoload
(define-minor-mode eshell-outline-mode
  "Outline-mode in Eshell.

\\{eshell-outline-mode-map}"		; doc
  nil					; init
  " $…"					; lighter
  eshell-outline-mode-map		; keymap
  (if eshell-outline-mode
      (progn
	(setq-local outline-regexp eshell-prompt-regexp)
	(add-to-invisibility-spec '(outline . t))
	;; TODO: how to make minor-mode only available in eshell-mode?
	(unless (derived-mode-p 'eshell-mode)
	  (eshell-outline-mode -1)
	  (user-error "Only enable this mode in eshell")))
    (remove-from-invisibility-spec '(outline . t))
    (outline-show-all)))

(provide 'eshell-outline)
;;; eshell-outline.el ends here
