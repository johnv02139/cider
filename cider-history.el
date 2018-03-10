;;; cider-history.el --- Basic REPL input history browser

;; Copyright (c) 2016 John Valente, Bozhidar Batsov and CIDER contributors

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Basic REPL input history browser for CIDER.

;;; Code:

(require 'cider-popup)
(require 'cider-compat)

(defvar cider-history-buffer "*cider-history*")

(push cider-history-buffer cider-ancillary-buffers)

(defvar cider-history-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map cider-popup-buffer-mode-map)
    (define-key map (kbd "RET") #'cider-history-execute-cmd-at-point)
    (define-key map (kbd "SPC") #'cider-history-insert-cmd-at-point)
    (define-key map "n" #'next-line)
    (define-key map "p" #'previous-line)
    map))

(defvar cider-history-mouse-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'cider-history-handle-mouse)
    map))

(define-derived-mode cider-history-mode clojure-mode "history"
  "Major mode for browsing the entries in CIDER's input history.

\\{cider-history-mode-map}"
  (setq buffer-read-only t)
  (setq-local electric-indent-chars nil)
  (setq-local truncate-lines t))


;; Everything works great as long as the commands are a single line.
;; When that's true, selecting the current command is simply a matter
;; of the text from the beginning of the line to the end.  Unfortunately,
;; Clojure commands often comprise more than one line.

;; There are a number of ways to try to handle this, but none of them
;; seem great.  As a first pass, I'm simply going to make sure that all
;; commands *do* fit on a single line, by replacing newlines with spaces.

;; That hack renders it not ready for "master", but ok for a branch.

(defun cider-history-command-text ()
  "Extract and return the command at point."
  (let ((bol (line-beginning-position))
        (eol (line-end-position)))
    (buffer-substring-no-properties bol eol)))

(defun format-command-for-cider-history (command)
  "Format the command so that `cider-history-command-text' can extract it."
  (concat
   ;; (replace-regexp-in-string "\n" " " command)
   command
   "\n"))

(defun get-latest-cider-hitory ()
  (buffer-local-value
   'cider-repl-input-history
   (cider-current-repl-buffer)))

(defun cider-history-populate-buffer (buffer)
  "Populate BUFFER with the cider REPL history."
  (with-current-buffer buffer
    (cider-history-mode)
    (let ((inhibit-read-only t)
          (commands (get-latest-cider-hitory))
          (line-num 1))
      (erase-buffer)
      (dolist (command commands)
        (insert (format ";;# %d\n" line-num))
        (setq line-num (+ line-num 1))
        (insert (format-command-for-cider-history command))))))

(defun cider-history-insert-cmd-at-point ()
  "Insert the command at point into the REPL."
  (interactive)
  (cider-insert-in-repl
   (cider-history-command-text)
   nil))

(defun cider-history-execute-cmd-at-point ()
  "Insert the command at point into the REPL and execute it."
  (interactive)
  (cider-insert-in-repl
   (cider-history-command-text)
   t))

(defun cider-history-handle-mouse (event)
  "Handle mouse click EVENT."
  (interactive "e")
  (cider-history-insert-cmd-at-point))

;;;###autoload
(defun cider-history ()
  "Insert all history entries into history buffer."
  (interactive)
  (with-current-buffer (cider-popup-buffer cider-history-buffer t)
    (cider-history-populate-buffer (current-buffer))
    (goto-char (point-min))
    (next-line)))

(provide 'cider-history)

;;; cider-history.el ends here
