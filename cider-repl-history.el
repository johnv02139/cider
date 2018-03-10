;;; cider-repl-history.el --- Basic REPL input history browser

;; Copyright (c) 2018 John Valente, Bozhidar Batsov and CIDER contributors

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

;; Unlike the checked-in version, which is based on browse-kill-ring,
;; and is fully featured, this is based on the CIDER classpath browser,
;; and is much simpler.

;;; Code:

(require 'cider-popup)
(require 'cider-compat)

(defvar cider-repl-history-buffer "*cider-repl-history*")

(push cider-repl-history-buffer cider-ancillary-buffers)

(defvar cider-repl-history-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map cider-popup-buffer-mode-map)
    (define-key map (kbd "RET") #'cider-repl-history-execute-cmd-at-point)
    (define-key map (kbd "SPC") #'cider-repl-history-insert-cmd-at-point)
    (define-key map "n" #'next-line)
    (define-key map "p" #'previous-line)
    map))

(defvar cider-repl-history-mouse-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'cider-repl-history-handle-mouse)
    map))

(define-derived-mode cider-repl-history-mode clojure-mode "history"
  "Major mode for browsing the entries in CIDER's input history.

\\{cider-repl-history-mode-map}"
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

(defun cider-repl-history-command-text ()
  "Extract and return the command at point."
  (let ((bol (line-beginning-position))
        (eol (line-end-position)))
    (buffer-substring-no-properties bol eol)))

(defun format-command-for-cider-repl-history (command)
  "Format the command so that `cider-repl-history-command-text' can extract it."
  (concat
   ;; (replace-regexp-in-string "\n" " " command)
   command
   "\n"))

(defun get-latest-cider-hitory ()
  (buffer-local-value
   'cider-repl-input-history
   (cider-current-repl-buffer)))

(defun cider-repl-history-populate-buffer (buffer)
  "Populate BUFFER with the cider REPL history."
  (with-current-buffer buffer
    (cider-repl-history-mode)
    (let ((inhibit-read-only t)
          (commands (get-latest-cider-hitory))
          (line-num 1))
      (erase-buffer)
      (dolist (command commands)
        (insert (format ";;# %d\n" line-num))
        (setq line-num (+ line-num 1))
        (insert (format-command-for-cider-repl-history command))))))

(defun cider-repl-history-insert-cmd-at-point ()
  "Insert the command at point into the REPL."
  (interactive)
  (cider-insert-in-repl
   (cider-repl-history-command-text)
   nil))

(defun cider-repl-history-execute-cmd-at-point ()
  "Insert the command at point into the REPL and execute it."
  (interactive)
  (cider-insert-in-repl
   (cider-repl-history-command-text)
   t))

(defun cider-repl-history-handle-mouse (event)
  "Handle mouse click EVENT."
  (interactive "e")
  (cider-repl-history-insert-cmd-at-point))

;;;###autoload
(defun cider-repl-history ()
  "Insert all history entries into history buffer."
  (interactive)
  (with-current-buffer (cider-popup-buffer cider-repl-history-buffer t)
    (cider-repl-history-populate-buffer (current-buffer))
    (goto-char (point-min))
    (next-line)))

(provide 'cider-repl-history)

;;; cider-repl-history.el ends here
