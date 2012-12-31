;;; keymap-naked.el --- keymap utilities that need library `naked.el'

;; Copyright (C) 2008-2012  Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This library contains additional keymap utilities that require the
;; library `naked.el'.

;;; Code:

(require 'cl-lib)
(require 'naked)

(declare-function save-sexp-save-generic "save-sexp")
(declare-function save-sexp-delete "save-sexp")
(declare-function save-sexp-prepare "save-sexp")

(defun kmu-define-key (keymap key def)
  "In KEYMAP, define key sequence KEY as DEF.
Like `define-key' but if KEY is a string it has to be in the
`naked' format without angle brackets."
  (define-key keymap
    (if (stringp key)
        (naked-read-kbd-macro key)
      key)
    def))

(defun kmu-remove-key (keymap key)
  "In KEYMAP, remove key sequence KEY.
If KEY is a string it has to be in the `naked' format without
angle brackets.

This removes the key sequence KEY from KEYMAP.  This differs from
using `undefined' or nil as definition.  `undefined' is a command
like any other and calling `define-key' with nil as definition
merely \"undefines\" the key by setting it to nil (but without
actually removing the event from the keymap)."
  (when (stringp key)
    (setq key (naked-read-kbd-macro key t)))
  (define-key keymap key nil)
  (if (> (length key) 1)
      ;; FIXME this assumes that e.g. (naked-read-kbd-macro "M-a")
      ;; return [27 97] but actually it returns [134217825].
      (delete (last (setq key (append key nil)))
              (lookup-key keymap (apply 'vector (butlast key))))
    (delete (cons (aref key 0) nil) keymap)))

(defmacro kmu-define-keys (mapvar feature &rest plist)
  "Define all keys in PLIST in the keymap stored in MAPVAR.
If a key in PLIST is a string it has to be in the `naked' format
without angle brackets.  `:remove' as a key definition means that
the existing definition (if any) should be removed from the keymap
using `kmu-remove-key'."
  (declare (indent 2))
  (if feature
      `(eval-after-load ',feature
         '(progn
            (when kmu-save-vanilla-keymaps-mode
              ;; `kmu-save-vanilla-keymaps' comes later in
              ;; `after-load-functions'.
              (kmu-save-vanilla-keymap ',mapvar))
            (kmu-define-keys-1 ',mapvar ',plist)))
    `(kmu-define-keys-1 ',mapvar ',plist)))

(defun kmu-define-keys-1 (keymap plist)
  (when (symbolp keymap)
    (setq keymap (symbol-value keymap)))
  (unless (keymapp keymap)
    (error "Not a keymap"))
  (while plist
    (unless (cdr plist)
      (error "Odd number of elements in PLIST"))
    (let ((key (pop plist))
          (def (pop plist)))
      (if (eq def :remove)
          (kmu-remove-key keymap key)
        (kmu-define-key keymap key def)))))

(defun save-kmu-define-keys (file mapvar feature bindings)
  (require 'sexp)
  (save-sexp-save-generic
   file
   (lambda (var)
     (if (not bindings)
	 (save-sexp-delete
	  (lambda (sexp)
	    (and (eq (nth 0 sexp) 'kmu-define-keys)
		 (eq (nth 1 sexp) var))))
       (save-sexp-prepare 'kmu-define-keys nil var)
       (princ " ")
       (prin1 feature)
       (dolist (b bindings)
	 (princ "\n  ")
	 (prin1 (car b))
	 (princ " ")
	 (prin1 (cadr b)))
       (forward-char)
       (backward-sexp)
       (prog1 (read (current-buffer))
	 (forward-sexp))))
   mapvar))

(defun kmu-naked-key-description (keys)
  "Like `naked-key-description' but also handle some special cases."
  (if (consp keys)
      ;; A string representation for character ranges.
      (let (prefix)
        (while (consp (cdr keys))
          (push (car keys) prefix)
          (setq keys (cdr keys)))
        (concat
         (when prefix
           (concat
            (kmu-naked-key-description (vconcat (nreverse prefix))) " "))
         (kmu-naked-key-description (vector (car keys))) ".."
         (kmu-naked-key-description (vector (cdr keys)))))
    ;; "Quote" certain events that cannot be encoded.
    (case (aref keys 0)
      (128     "128")
      (4194303 "255")
      (t
       ;; Merge ESC into following event.
       (let ((s (naked-key-description keys)))
         (while (and (string-match
                      "\\(ESC \\(C-\\)?\\([^ ]+\\)\\)" s)
                     (save-match-data
                       (not (string-match "\\(ESC\\|M-\\)"
                                          (match-string 3 s)))))
           (setq s (replace-match "\\2M-\\3" t nil s 1)))
         s)))))

(defun kmu-map-naked-keymap (function keymap)
  "Call FUNCTION once for each event sequence binding in KEYMAP.
FUNCTION is called with two arguments: an event sequence string
as returned by `naked-key-description', and the definition the
last event in that sequence it is bound to.

When the definition is another keymap then instead of calling
FUNCTION with that event and it's definition once, FUNCTION is
recursively called with each separate event sequence and it's
non-keymap definition.

If the last event in an event sequence is actually a character
range then that is represented as \"START .. END\".  As usual
this might be preceded by other events leading to the character
range."
  (kmu-map-keymap
   `(lambda (key def)
      (funcall ,function (kmu-naked-key-description key) def))
   keymap))

(provide 'keymap-naked)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; keymap-naked.el ends here
