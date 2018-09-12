;;; cider-repl-history.el --- REPL input history browser

;; Copyright (c) 2016 John Valente and browse-kill-ring authors

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

;; Based heavily on browse-kill-ring:
;; ----------------------------------
;; Copyright (C) 2001, 2002 Colin Walters <walters@verbum.org>
;; Author: Colin Walters <walters@verbum.org>
;; Maintainer: browse-kill-ring <browse-kill-ring@tonotdo.com>
;; Created: 7 Apr 2001
;; Version: 2.0.0
;; URL: https://github.com/browse-kill-ring/browse-kill-ring

;;; Commentary:

;; REPL input history browser for CIDER.

;; Allows you to browse the full input history for your REPL buffer, and
;; insert previous commands at the prompt.

;;; Code:

(require 'cl-lib)
(require 'cider-compat)
(require 'cider-popup)
(require 'derived)
(require 'pulse)

(defconst cider-history-buffer "*cider-repl-history*")
(add-to-list 'cider-ancillary-buffers cider-history-buffer)

(defgroup cider-repl-history nil
  "A package for browsing and inserting the items in the CIDER command history."
  :link '(url-link "https://github.com/johnv02139/cider/tree/historycmd")
  :group 'convenience)

(defvar cider-history-display-styles
  '((separated . cider-history-insert-as-separated)
    (one-line . cider-history-insert-as-one-line)))

(defcustom cider-history-display-style 'separated
  "How to display the CIDER command history items.

If `one-line', then replace newlines with \"\\n\" for display.

If `separated', then display `cider-history-separator' between
entries."
  :type '(choice (const :tag "One line" one-line)
                 (const :tag "Separated" separated))
  :group 'cider-repl-history
  :package-version '(cider . "0.15.0"))

(defcustom cider-history-quit-action 'quit-window
  "What action to take when `cider-history-quit' is called.

If `bury-buffer', then simply bury the *cider-repl-history* buffer, but keep
the window.

If `bury-and-delete-window', then bury the buffer, and (if there is
more than one window) delete the window.

If `delete-and-restore', then restore the window configuration to what it was
before `cider-repl-history' was called, and kill the *cider-repl-history* buffer.

If `quit-window', then restore the window configuration to what
it was before `cider-repl-history' was called, and bury *cider-repl-history*.
This is the default.

If `kill-and-delete-window', then kill the *cider-repl-history* buffer, and
delete the window on close.

Otherwise, it should be a function to call."
  ;; Note, if you use one of the non-"delete" options, after you "quit",
  ;; the *cider-repl-history* buffer is still available.  If you are using
  ;; `cider-history-show-preview', and you switch to *cider-repl-history* (i.e.,
  ;; with C-x b), it will not give the preview unless and until you "update"
  ;; the *cider-repl-history* buffer.
  ;;
  ;; This really should not be an issue, because there's no reason to "switch"
  ;; back to the buffer.  If you want to get it back, you can just do C-c M-p
  ;; from the REPL buffer.

  ;; If you get in this situation and find it annoying, you can either disable
  ;; the preview, or set `cider-history-quit-action' to 'delete-and-restore.
  ;; Then you will simply not have the *cider-repl-history* buffer after you quit,
  ;; and it won't be an issue.

  :type '(choice (const :tag "Bury buffer"
                        :value bury-buffer)
                 (const :tag "Bury buffer and delete window"
                        :value bury-and-delete-window)
                 (const :tag "Delete window"
                        :value delete-and-restore)
                 (const :tag "Save and restore"
                        :value quit-window)
                 (const :tag "Kill buffer and delete window"
                        :value kill-and-delete-window)
                 function)
  :group 'cider-repl-history
  :package-version '(cider . "0.15.0"))

(defcustom cider-history-resize-window nil
  "Whether to resize the `cider-repl-history' window to fit its contents.
Value is either t, meaning yes, or a cons pair of integers,
 (MAXIMUM . MINIMUM) for the size of the window.  MAXIMUM defaults to
the window size chosen by `pop-to-buffer'; MINIMUM defaults to
`window-min-height'."
  :type '(choice (const :tag "No" nil)
                 (const :tag "Yes" t)
                 (cons (integer :tag "Maximum") (integer :tag "Minimum")))
  :group 'cider-repl-history
  :package-version '(cider . "0.15.0"))

(defcustom cider-history-separator ";;;;;;;;;;"
  "The string separating entries in the `separated' style.
See `cider-history-display-style'."
  ;; The (default) separator is a Clojure comment, to preserve fontification
  ;; in the buffer.
  :type 'string
  :group 'cider-repl-history
  :package-version '(cider . "0.15.0"))

(defcustom cider-history-recenter nil
  "If non-nil, then always keep the current entry at the top of the window."
  :type 'boolean
  :group 'cider-repl-history
  :package-version '(cider . "0.15.0"))

(defcustom cider-history-highlight-current-entry nil
  "If non-nil, highlight the currently selected command history entry."
  :type 'boolean
  :group 'cider-repl-history
  :package-version '(cider . "0.15.0"))

(defcustom cider-history-highlight-inserted-item
  cider-history-highlight-current-entry
  "If non-nil, then temporarily highlight the inserted command history entry.
The value selected controls how the inserted item is highlighted,
possible values are `solid' (highlight the inserted text for a
fixed period of time), or `pulse' (fade out the highlighting gradually).
Setting this variable to the value t will select the default
highlighting style, which currently `pulse'.

The variable `cider-history-inserted-item-face' contains the
face used for highlighting."
  :type '(choice (const nil) (const t) (const solid) (const pulse))
  :group 'cider-repl-history
  :package-version '(cider . "0.15.0"))

(defcustom cider-history-separator-face 'bold
  "The face in which to highlight the `cider-history-separator'."
  :type 'face
  :group 'cider-repl-history
  :package-version '(cider . "0.15.0"))

(defcustom cider-history-current-entry-face 'highlight
  "The face in which to highlight the command history current entry."
  :type 'face
  :group 'cider-repl-history
  :package-version '(cider . "0.15.0"))

(defcustom cider-history-inserted-item-face 'highlight
  "The face in which to highlight the inserted item."
  :type 'face
  :group 'cider-repl-history
  :package-version '(cider . "0.15.0"))

(defcustom cider-history-maximum-display-length nil
  "Whether or not to limit the length of displayed items.

If this variable is an integer, the display of the command history will be
limited to that many characters.
Setting this variable to nil means no limit."
  :type '(choice (const :tag "None" nil)
                 integer)
  :group 'cider-repl-history
  :package-version '(cider . "0.15.0"))

(defcustom cider-history-display-duplicates t
  "If non-nil, then display duplicate items in the command history."
  :type 'boolean
  :group 'cider-repl-history
  :package-version '(cider . "0.15.0"))

(defcustom cider-history-display-leftmost-duplicate t
  "When `cider-history-display-duplicates' is nil, then display leftmost (last) duplicate items in the command history."
  :type 'boolean
  :group 'cider-repl-history
  :package-version '(cider . "0.15.0"))

(defcustom cider-history-text-properties nil
  "If non-nil, maintain text properties of the command history items."
  :type 'boolean
  :group 'cider-repl-history
  :package-version '(cider . "0.15.0"))

(defcustom cider-history-hook nil
  "A list of functions to call after `cider-repl-history'."
  :type 'hook
  :group 'cider-repl-history
  :package-version '(cider . "0.15.0"))

(defcustom cider-history-show-preview nil
  "If non-nil, show a preview of the inserted text in the REPL buffer.

The REPL buffer would show a preview of what the buffer would look like
if the item under point were inserted."

  :type 'boolean
  :group 'cider-repl-history
  :package-version '(cider . "0.15.0"))

(defvar cider-history-repl-window nil
  "The window in which chosen command history data will be inserted.
It is probably not a good idea to set this variable directly; simply
call `cider-repl-history' again.")

(defvar cider-history-repl-buffer nil
  "The buffer in which chosen command history data will be inserted.
It is probably not a good idea to set this variable directly; simply
call `cider-repl-history' again.")

(defvar cider-history-preview-overlay nil
  "The overlay used to preview what would happen if the user inserted the given text.")

(defvar cider-history-previous-overlay nil
  "Previous overlay within *cider-repl-history* buffer.")


(defun cider-history-get-history ()
  "Function to retrieve history from the REPL buffer."
  (if cider-history-repl-buffer
      (buffer-local-value
       'cider-repl-input-history
       cider-history-repl-buffer)
    (error "Variable `cider-history-repl-buffer' not bound to a buffer")))

(defun cider-history-resize-window ()
  "If variable `cider-history-resize-window' is non-nil, resize the *cider-repl-history* window."
  (when cider-history-resize-window
    (apply #'fit-window-to-buffer (selected-window)
           (if (consp cider-history-resize-window)
               (list (car cider-history-resize-window)
                     (or (cdr cider-history-resize-window)
                         window-min-height))
             (list nil window-min-height)))))

(defun cider-history-read-regexp (msg use-default-p)
  "Get a regular expression from the user, prompting with MSG; previous entry is default if USE-DEFAULT-P."
  (let* ((default (car regexp-history))
         (input
          (read-from-minibuffer
           (if (and default use-default-p)
               (format "%s for regexp (default `%s'): "
                       msg
                       default)
             (format "%s (regexp): " msg))
           nil
           nil
           nil
           'regexp-history
           (if use-default-p nil default))))
    (if (equal input "")
        (if use-default-p default nil)
      input)))

(defun cider-history-clear-preview ()
  "Clear the preview, if one is present."
  (interactive)
  (when cider-history-preview-overlay
    (cl-assert (overlayp cider-history-preview-overlay))
    (delete-overlay cider-history-preview-overlay)))

(defun cider-history-cleanup-on-exit ()
  "Function called when the user is finished with `cider-repl-history'.
This function performs any cleanup that is required when the user
has finished interacting with the *cider-repl-history* buffer.  For now
the only cleanup performed is to remove the preview overlay, if
it's turned on."
  (cider-history-clear-preview))

(defun cider-history-quit ()
  "Take the action specified by `cider-history-quit-action'."
  (interactive)
  (cider-history-cleanup-on-exit)
  (pcase cider-history-quit-action
    (`delete-and-restore
     (quit-restore-window (selected-window) 'kill))
    (`quit-window
     (quit-window))
    (`kill-and-delete-window
     (kill-buffer (current-buffer))
     (unless (= (count-windows) 1)
       (delete-window)))
    (`bury-and-delete-window
     (bury-buffer)
     (unless (= (count-windows) 1)
       (delete-window)))
    (_
     (funcall cider-history-quit-action))))

(defun cider-history-preview-overlay-setup (orig-buf)
  "Setup the preview overlay in ORIG-BUF."
  (when cider-history-show-preview
    (with-current-buffer orig-buf
      (let* ((will-replace (region-active-p))
             (start (if will-replace
                        (min (point) (mark))
                      (point)))
             (end (if will-replace
                      (max (point) (mark))
                    (point))))
        (cider-history-clear-preview)
        (setq cider-history-preview-overlay
              (make-overlay start end orig-buf))
        (overlay-put cider-history-preview-overlay
                     'invisible t)))))

;; UNUSED
;; The "pulse" functionality, part of cedet, was once an optional downloaded
;; package.  But Cider 0.15 requires Emacs 24.4+, which includes cedet by
;; default, so we should not need to check it.
(defun cider-history-adjust-insert ()
  "If we want `pulse', try to load the library, and change highlight-style if unsuccessful."
  (when (or (eq cider-history-highlight-inserted-item 'pulse)
            (eq cider-history-highlight-inserted-item 't))
    (unless (and (require 'pulse nil t)
                 (fboundp 'pulse-momentary-highlight-region))
      (warn "Unable to load `pulse' library")
      (setq cider-history-highlight-inserted-item 'solid))))

(defun cider-history-highlight-inserted (start end)
  "Insert the text between START and END."
  ;; (when cider-history-highlight-inserted-item (cider-history-adjust-insert))
  (pcase cider-history-highlight-inserted-item
    ((or `pulse `t)
     (let ((pulse-delay .05) (pulse-iterations 10))
       (with-no-warnings
         (pulse-momentary-highlight-region
          start end cider-history-inserted-item-face))))
    (`solid
     (let ((o (make-overlay start end)))
       (overlay-put o 'face cider-history-inserted-item-face)
       (sit-for 0.5)
       (delete-overlay o)))))

(defun cider-history-insert-and-highlight (str)
  "Helper function to insert STR at point, highlighting it if appropriate."
  (let ((before-insert (point)))
    (let (deactivate-mark)
      (insert-for-yank str))
    (cider-history-highlight-inserted
     before-insert
     (point))))

(defun cider-history-target-overlay-at (position &optional no-error)
  "Return overlay at POSITION that has property `cider-history-target'.
If no such overlay, raise an error unless NO-ERROR is true, in which
case retun nil."
  (let ((ovs  (overlays-at (point))))
    (catch 'cider-history-target-overlay-at
      (dolist (ov ovs)
        (when (overlay-get ov 'cider-history-target)
          (throw 'cider-history-target-overlay-at ov)))
      (unless no-error
        (error "No CIDER history item here")))))

(defun cider-history-current-string (pt &optional no-error)
  "Find the string to insert into the REPL by looking for the overlay at PT; might error unless NO-ERROR set."
  (let ((o (cider-history-target-overlay-at pt t)))
    (if o
        (overlay-get o 'cider-history-target)
      (unless no-error
        (error "No CIDER history item in this buffer")))))

(defun cider-history-do-insert (buf pt)
  "Helper function to insert text from BUF at PT into the REPL buffer and kill *cider-repl-history*."
  ;; Note: as mentioned at the top, this file is based on browse-kill-ring,
  ;; which has numerous insertion options.  The functionality of
  ;; browse-kill-ring allows users to insert at point, and move point to the end
  ;; of the inserted text; or insert at the beginning or end of the buffer,
  ;; while leaving point alone.  And each of these had the option of leaving the
  ;; history buffer in place, or getting rid of it.  That was appropriate for a
  ;; generic paste tool, but for inserting a previous command into an
  ;; interpreter, I felt the only useful option would be inserting it at the end
  ;; and quitting the history buffer, so that is all that's provided.
  (let ((str (cider-history-current-string pt)))
    (cider-history-quit)
    (with-selected-window cider-history-repl-window
      (with-current-buffer cider-history-repl-buffer
        (let ((max (point-max)))
          (if (= max (point))
              (cider-history-insert-and-highlight str)
            (save-excursion
              (goto-char max)
              (cider-history-insert-and-highlight str))))))))

(defun cider-history-insert-and-quit ()
  "Insert the item into the REPL buffer, and close the *cider-repl-history* buffer.

The text is always inserted at the very bottom of the REPL buffer.  If your
cursor is already at the bottom, it is advanced to the end of the inserted
text.  If your cursor is somewhere else, the cursor is not moved, but the
text is still inserted at the end."
  (interactive)
  (cider-history-do-insert (current-buffer) (point)))

(defun cider-history-mouse-insert (e)
  "Insert the item at E into the REPL buffer, and close the *cider-repl-history*.

The text is always inserted at the very bottom of the REPL buffer.  If your
cursor is already at the bottom, it is advanced to the end of the inserted
text.  If your cursor is somewhere else, the cursor is not moved, but the
text is still inserted at the end."
  (interactive "e")
  (let* ((data (save-excursion
                 (mouse-set-point e)
                 (cons (current-buffer) (point))))
         (buf (car data))
         (pt (cdr data)))
    (cider-history-do-insert buf pt)))

(defun cider-history-clear-highlighed-entry ()
  "Clear the highlighted entry, when one exists."
  (when cider-history-previous-overlay
    (cl-assert (overlayp cider-history-previous-overlay)
               t "not an overlay")
    (overlay-put cider-history-previous-overlay 'face nil)))

(defun cider-history-update-highlighed-entry ()
  "Update highlighted entry, when feature is turned on."
  (when cider-history-highlight-current-entry
    (if-let ((current-overlay (cider-history-target-overlay-at (point) t)))
        (unless (equal cider-history-previous-overlay current-overlay)
          ;; We've changed overlay.  Clear current highlighting,
          ;; and highlight the new overlay.
          (cl-assert (overlay-get current-overlay 'cider-history-target) t)
          (cider-history-clear-highlighed-entry)
          (setq cider-history-previous-overlay current-overlay)
          (overlay-put current-overlay 'face
                       cider-history-current-entry-face))
      ;; No overlay at point.  Just clear all current highlighting.
      (cider-history-clear-highlighed-entry))))

(defun cider-history-forward (&optional arg)
  "Move forward by ARG command history entries."
  (interactive "p")
  (beginning-of-line)
  (while (not (zerop arg))
    (let ((o (cider-history-target-overlay-at (point) t)))
      (cond
       ((>= arg 0)
        (decf arg)
        ;; We're on a cider-repl-history overlay, skip to the end of it.
        (when o
          (goto-char (overlay-end o))
          (setq o nil))
        (while (not (or o (eobp)))
          (goto-char (next-overlay-change (point)))
          (setq o (cider-history-target-overlay-at (point) t))))
       (t
        (incf arg)
        (when o
          (goto-char (overlay-start o))
          (setq o nil))
        (while (not (or o (bobp)))
          (goto-char (previous-overlay-change (point)))
          (setq o (cider-history-target-overlay-at (point) t)))))))
  (when cider-history-recenter
    (recenter 1)))

(defun cider-history-previous (&optional arg)
  "Move backward by ARG command history entries."
  (interactive "p")
  (cider-history-forward (- arg)))

(defun cider-history-search-forward (regexp &optional backwards)
  "Move to the next command history entry matching REGEXP from point.
If optional arg BACKWARDS is non-nil, move to the previous matching
entry."
  (interactive
   (list (cider-history-read-regexp "Search forward" t)
         current-prefix-arg))
  (let ((orig (point)))
    (cider-history-forward (if backwards -1 1))
    (let ((over (cider-history-target-overlay-at (point) t)))
      (while (and over
                  (not (if backwards (bobp) (eobp)))
                  (not (string-match regexp
                                     (overlay-get over
                                                  'cider-history-target))))
        (cider-history-forward (if backwards -1 1))
        (setq over (cider-history-target-overlay-at (point) t)))
      (unless (and over
                   (string-match regexp
                                 (overlay-get over
                                              'cider-history-target)))
        (goto-char orig)
        (message "No more command history entries matching %s" regexp)))))

(defun cider-history-search-backward (regexp)
  "Move to the previous command history entry matching REGEXP from point."
  (interactive
   (list (cider-history-read-regexp "Search backward" t)))
  (cider-history-search-forward regexp t))

(defun cider-history-elide (str)
  "If STR is too long, abbreviate it with an ellipsis; otherwise, return it unchanged."
  (if (and cider-history-maximum-display-length
           (> (length str)
              cider-history-maximum-display-length))
      (concat (substring str 0 (- cider-history-maximum-display-length 3))
              (propertize "..." 'cider-history-extra t))
    str))

(defmacro cider-history-add-overlays-for (item &rest body)
  "Add overlays for ITEM, and execute BODY."
  (let ((beg (cl-gensym "cider-history-add-overlays-"))
        (end (cl-gensym "cider-history-add-overlays-")))
    `(let ((,beg (point))
           (,end
            (progn
              ,@body
              (point))))
       (let ((o (make-overlay ,beg ,end)))
         (overlay-put o 'cider-history-target ,item)
         (overlay-put o 'mouse-face 'highlight)))))

(defun cider-history-insert-as-separated (items)
  "Insert ITEMS into the current buffer, with separators between items."
  (while items
    (let* ((origitem (car items))
           (item (cider-history-elide origitem))
           (len (length item)))
      (cider-history-add-overlays-for origitem (insert item))
      ;; When the command history has items with read-only text property at
      ;; **the end of** string, cider-history-setup fails with error
      ;; `Text is read-only'.  So inhibit-read-only here.
      ;; See http://bugs.debian.org/225082
      (let ((inhibit-read-only t))
        (insert "\n")
        (when (cdr items)
          (insert (propertize cider-history-separator
                              'cider-history-extra t
                              'cider-history-separator t))
          (insert "\n"))))
    (setq items (cdr items))))

(defun cider-history-insert-as-one-line (items)
  "Insert ITEMS into the current buffer, formatting each item as a single line.

An explicit newline character will replace newlines so that the text retains its
spacing when it's actually inserted into the REPL buffer."
  (dolist (item items)
    (cider-history-add-overlays-for
     item
     (let* ((item (cider-history-elide item))
            (len (length item))
            (start 0)
            (newl (propertize "\\n" 'cider-history-extra t)))
       (while (and (< start len)
                   (string-match "\n" item start))
         (insert (substring item start (match-beginning 0))
                 newl)
         (setq start (match-end 0)))
       (insert (substring item start len))))
    (insert "\n")))

(defun cider-history-preview-update-text (preview-text)
  "Update `cider-history-preview-overlay' to show `PREVIEW-TEXT`."
  ;; If preview-text is nil, replacement should be nil too.
  (cl-assert (overlayp cider-history-preview-overlay))
  (let ((replacement (when preview-text
                       (propertize preview-text 'face 'highlight))))
    (overlay-put cider-history-preview-overlay
                 'before-string replacement)))

(defun cider-history-preview-update-by-position (&optional pt)
  "Update `cider-history-preview-overlay' to match item at PT.

This function is called whenever the selection in the *cider-repl-history*
buffer is adjusted, the `cider-history-preview-overlay'
is udpated to preview the text of the selection at PT (or the
current point if not specified)."
  (let ((new-text (cider-history-current-string
                   (or pt (point)) t)))
    (cider-history-preview-update-text new-text)))

(defun cider-history-undo-other-window ()
  "Undo the most recent change in the other window's buffer.
You most likely want to use this command for undoing an insertion of
text from the *cider-repl-history* buffer."
  (interactive)
  (with-current-buffer cider-history-repl-buffer
    (undo)))

(defun cider-history-setup (repl-win repl-buf history-buf &optional regexp)
  "Setup: REPL-WIN and REPL-BUF are where to insert commands, HISTORY-BUF is the history, and optional arg REGEXP is a filter."
  (cider-history-preview-overlay-setup repl-buf)
  (with-current-buffer history-buf
    (unwind-protect
        (progn
          (cider-history-mode)
          (setq buffer-read-only nil)
          (when (eq 'one-line cider-history-display-style)
            (setq truncate-lines t))
          (let ((inhibit-read-only t))
            (erase-buffer))
          (setq cider-history-repl-buffer repl-buf)
          (setq cider-history-repl-window repl-win)
          (let* ((cider-history-maximum-display-length
                  (if (and cider-history-maximum-display-length
                           (<= cider-history-maximum-display-length 3))
                      4
                    cider-history-maximum-display-length))
                 (cider-command-history (cider-history-get-history))
                 (items (mapcar
                         (if cider-history-text-properties
                             #'copy-sequence
                           #'substring-no-properties)
                         cider-command-history)))
            (when (not cider-history-display-duplicates)
              ;; display leftmost or rightmost duplicate.
              ;; if `cider-history-display-leftmost-duplicate' is t,
              ;; display leftmost (last) duplicate.
              (cl-delete-duplicates
               items
               :test #'equal
               :from-end cider-history-display-leftmost-duplicate))
            (when (stringp regexp)
              (setq items (delq nil
                                (mapcar
                                 #'(lambda (item)
                                     (when (string-match regexp item)
                                       item))
                                 items))))
            (funcall (or (cdr (assq cider-history-display-style
                                    cider-history-display-styles))
                         (error "Invalid `cider-history-display-style': %s"
                                cider-history-display-style))
                     items)
            (when cider-history-show-preview
              (cider-history-preview-update-by-position (point-min))
              ;; Local post-command-hook, only happens in *cider-repl-history*
              (add-hook 'post-command-hook
                        'cider-history-preview-update-by-position
                        nil t)
              (add-hook 'kill-buffer-hook
                        'cider-history-cleanup-on-exit
                        nil t))
            (when cider-history-highlight-current-entry
              (add-hook 'post-command-hook
                        'cider-history-update-highlighed-entry
                        nil t))
            (message
             (let ((entry (if (= 1 (length cider-command-history))
                              "entry"
                            "entries")))
               (concat
                (if (and (not regexp)
                         cider-history-display-duplicates)
                    (format "%s %s in the command history."
                            (length cider-command-history) entry)
                  (format "%s (of %s) %s in the command history shown."
                          (length items) (length cider-command-history) entry))
                (substitute-command-keys
                 (concat "  Type \\[cider-history-quit] to quit.  "
                         "\\[describe-mode] for help.")))))
            (set-buffer-modified-p nil)
            (goto-char (point-min))
            (cider-history-forward 0)
            (setq mode-name (if regexp
                                (concat "History [" regexp "]")
                              "History"))
            (run-hooks 'cider-history-hook)))
      (setq buffer-read-only t))))

(defun cider-history-update ()
  "Update the history buffer to reflect the latest state of the command history."
  (interactive)
  (cl-assert (eq major-mode 'cider-history-mode))
  (cider-history-setup cider-history-repl-window
                       cider-history-repl-buffer
                       (current-buffer))
  (cider-history-resize-window))

(defun cider-history-occur (regexp)
  "Display all command history entries matching REGEXP."
  (interactive
   (list (cider-history-read-regexp
          "Display command history entries matching" nil)))
  (cl-assert (eq major-mode 'cider-history-mode))
  (cider-history-setup cider-history-repl-window
                       cider-history-repl-buffer
                       (current-buffer)
                       regexp)
  (cider-history-resize-window))

(put 'cider-history-mode 'mode-class 'special)
(define-derived-mode cider-history-mode clojure-mode "History"
  "Major mode for browsing the entries in the command input history.

\\{cider-history-mode-map}"
  (define-key cider-history-mode-map (kbd "n") 'cider-history-forward)
  (define-key cider-history-mode-map (kbd "p") 'cider-history-previous)
  (define-key cider-history-mode-map (kbd "SPC") 'cider-history-insert-and-quit)
  (define-key cider-history-mode-map (kbd "RET") 'cider-history-insert-and-quit)
  (define-key cider-history-mode-map [(mouse-2)] 'cider-history-mouse-insert)
  (define-key cider-history-mode-map (kbd "l") 'cider-history-occur)
  (define-key cider-history-mode-map (kbd "s") 'cider-history-search-forward)
  (define-key cider-history-mode-map (kbd "r") 'cider-history-search-backward)
  (define-key cider-history-mode-map (kbd "g") 'cider-history-update)
  (define-key cider-history-mode-map (kbd "q") 'cider-history-quit)
  (define-key cider-history-mode-map (kbd "U") 'cider-history-undo-other-window)
  (define-key cider-history-mode-map (kbd "?") 'describe-mode)
  (define-key cider-history-mode-map (kbd "h") 'describe-mode))

;;;###autoload
(defun cider-repl-history ()
  "Display items in the CIDER command history in another buffer."
  (interactive)
  (when (eq major-mode 'cider-history-mode)
    (error "Already viewing the CIDER command history"))

  (let* ((repl-win (selected-window))
         (repl-buf (window-buffer repl-win))
         (buf (get-buffer-create cider-history-buffer)))
    (cider-history-setup repl-win repl-buf buf)
    (pop-to-buffer buf)
    (cider-history-resize-window)))

(provide 'cider-repl-history)

;;; cider-repl-history.el ends here
