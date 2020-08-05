;;; eshell-outline.el --- View eshell buffer in outline-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Jamie Beardslee

;; Author: Jamie Beardslee <jdb@jamzattack.xyz>
;; Keywords:

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

The command `eshell-outline-minor-mode' is a more interactive
version, with more specialized keybindings."
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
  "Return t if point is at the latest input."
  (save-excursion
    (not (eshell-previous-prompt 1))))

(defun eshell-outline-hide-or-interrupt (&optional int)
  (interactive "P")
  (if (or int eshell-process-list)
      (eshell-interrupt-process)
    (outline-hide-entry)))

(defun eshell-outline-hide-or-kill (&optional kill)
  (interactive "P")
  (if (or kill eshell-process-list)
      (eshell-kill-process)
    (outline-show-branches)))


;;; Keymap

(setq eshell-outline-minor-mode-map
      ;; eshell-{previous,next}-prompt are the same as
      ;; outline-{next,previous} -- no need to bind these.
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'eshell-outline-hide-or-interrupt)
    (define-key map (kbd "C-c C-k") #'eshell-outline-hide-or-kill)

    ;; From outline.el
    (define-key map (kbd "C-c C-a") #'outline-show-all)
    (define-key map (kbd "C-c C-e") #'outline-show-entry)
    (define-key map (kbd "C-c C-s") #'outline-show-subtree)
    (define-key map (kbd "C-c C-t") #'outline-hide-body)

    ;; Default `outline-minor-mode' keybindings
    (define-key map (kbd "C-c @") outline-mode-prefix-map)
    map))

(define-minor-mode eshell-outline-minor-mode
  "Outline-mode in Eshell.

\\{eshell-outline-minor-mode-map}" nil " $…"
  eshell-outline-minor-mode-map
  (unless (derived-mode-p 'eshell-mode)
    (user-error "Only enable this mode in eshell"))
  (if eshell-outline-minor-mode
      (progn
	(setq-local outline-regexp eshell-prompt-regexp)
	(add-to-invisibility-spec '(outline . t)))
    (remove-from-invisibility-spec '(outline . t))
    (outline-show-all)))

(provide 'eshell-outline)
;;; eshell-outline.el ends here
