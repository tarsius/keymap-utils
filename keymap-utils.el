;;; keymap-utils.el --- keymap utilities

;; Copyright (C) 2008-2012  Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Created: 20080830
;; Version: 0.4.4
;; Package-Requires: ((cl-lib "0.2"))
;; Homepage: https://github.com/tarsius/keymap-utils
;; Keywords: convenience, extensions

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

;; This package provides some utilities useful for inspecting and
;; modifying keymaps.

;;; Code:

(require 'cl-lib)
(require 'naked)

(declare-function save-sexp-save-generic "save-sexp")
(declare-function save-sexp-delete "save-sexp")
(declare-function save-sexp-prepare "save-sexp")

;;; Predicates.

(defun kmu-keymap-variable-p (object)
  "Return t if OBJECT is a symbol whose variable definition is a keymap."
  (and (symbolp object)
       (boundp  object)
       (keymapp (symbol-value object))))

(defun kmu-keymap-list-p (object)
  "Return t if OBJECT is a list whose first element is the symbol `keymap'."
  (and (listp   object)
       (keymapp object)))

(defun kmu-prefix-command-p (object &optional boundp)
  "Return non-nil if OBJECT is a symbol whose function definition is a keymap.
The value returned is the keymap stored as OBJECTS variable definition or
else the variable which holds the keymap."
  (and (symbolp object)
       (fboundp object)
       (keymapp (symbol-function object))
       (if (and (boundp  object)
                (keymapp (symbol-value object)))
           (symbol-value object)
         (kmu-keymap-variable (symbol-function object)))))

(defun kmu-full-keymap-p (object)
  "Return t if OBJECT is a full keymap.
A full keymap is a keymap whose second element is a char-table."
  (if (kmu-prefix-command-p object)
      (char-table-p (cadr (symbol-function object)))
    (and (keymapp object)
         (char-table-p (cadr object)))))

(defun kmu-sparse-keymap-p (object)
  "Return t if OBJECT is a sparse keymap.
A sparse keymap is a keymap whose second element is not a char-table."
  (if (kmu-prefix-command-p object)
      (not (char-table-p (cadr (symbol-function object))))
    (and (keymapp object)
         (not (char-table-p (cadr object))))))

;;; Key Lookup.

(defun kmu-lookup-local-key (keymap key &optional accept-default)
  "In KEYMAP, look up key sequence KEY.  Return the definition.

Unlike `lookup-key' (which see) this doesn't consider bindings made
in KEYMAP's parent keymap."
  (lookup-key (kmu--strip-keymap keymap) key accept-default))

(defun kmu-lookup-parent-key (keymap key &optional accept-default)
  "In KEYMAP's parent keymap, look up key sequence KEY.
Return the definition.

Unlike `lookup-key' (which see) this only conciders bindings made in
KEYMAP's parent keymap and recursivly all parent keymaps of keymaps
events in KEYMAP are bound to."
  (lookup-key (kmu--collect-parmaps keymap) key accept-default))

(defun kmu--strip-keymap (keymap)
  "Return a copy of KEYMAP with all parent keymaps removed.

This not only removes the parent keymap of KEYMAP but also recursively
the parent keymap of any keymap a key in KEYMAP is bound to."
  (cl-labels ((strip-keymap
               (keymap)
               (set-keymap-parent keymap nil)
               (cl-loop for key being the key-code of keymap
                        using (key-binding binding) do
                        (and (keymapp binding)
                             (not (kmu-prefix-command-p binding))
                             (strip-keymap binding)))
               keymap))
    (strip-keymap (copy-keymap keymap))))

(defun kmu--collect-parmaps (keymap)
  "Return a copy of KEYMAP with all local bindings removed."
  (cl-labels ((collect-parmaps
               (keymap)
               (let ((new-keymap (make-sparse-keymap)))
                 (set-keymap-parent new-keymap (keymap-parent keymap))
                 (set-keymap-parent keymap nil)
                 (cl-loop for key being the key-code of keymap
                          using (key-binding binding) do
                          (and (keymapp binding)
                               (not (kmu-prefix-command-p binding))
                               (define-key new-keymap (vector key)
                                 (collect-parmaps binding))))
                 new-keymap)))
    (collect-parmaps (copy-keymap keymap))))

;;; Keymap Variables.

(defun kmu-keymap-variable (keymap &rest exclude)
  "Return a symbol whose value is KEYMAP.

Comparison is done with `eq'.  If there are multiple variables
whose value is KEYMAP it is undefined which is returned.

Ignore symbols listed in optional EXCLUDE.  Use this to prevent a
symbol from being returned which is dynamically bound to KEYMAP."
  (when (keymapp keymap)
    (setq exclude (append '(keymap --match-- --symbol--) exclude))
    (let (--match--)
      (cl-do-symbols (--symbol--)
        (and (not (memq --symbol-- exclude))
             (boundp --symbol--)
             (eq (symbol-value --symbol--) keymap)
             (setq --match-- --symbol--)
             (cl-return nil)))
      --match--)))

(defun kmu-keymap-prefix-command (keymap)
  "Return a symbol whose function definition is KEYMAP.

Comparison is done with `eq'.  If there are multiple symbols
whose function definition is KEYMAP it is undefined which is
returned."
  (when (keymapp keymap)
    (let (--match--)
      (cl-do-symbols (--symbol--)
        (and (fboundp --symbol--)
             (eq (symbol-function --symbol--) keymap)
             (setq --match-- --symbol--)
             (cl-return nil)))
      --match--)))

(defun kmu-keymap-parent (keymap &optional need-symbol &rest exclude)
  "Return the parent keymap of KEYMAP.

If a variable exists whose value is KEYMAP's parent keymap return
that.  Otherwise if KEYMAP does not have a parent keymap return
nil.  Otherwise if KEYMAP has a parent keymap but no variable is
bound to it return the parent keymap, unless optional NEED-SYMBOL
is non-nil in which case nil is returned.

Comparison is done with `eq'.  If there are multiple variables
whose value is the keymap it is undefined which is returned.

Ignore symbols listed in optional EXCLUDE.  Use this to prevent
a symbol from being returned which is dynamically bound to the
parent keymap."
  (let ((--parmap-- (keymap-parent keymap)))
    (when --parmap--
      (or (kmu-keymap-variable --parmap-- '--parmap--)
          (unless need-symbol --parmap--)))))

(defun kmu-mapvar-list (&optional exclude-prefix-commands)
  "Return a list of all keymap variables.

If optional EXCLUDE-PREFIX-COMMANDS is non-nil exclude all
variables whose variable definition is also the function
definition of a prefix command."
  (let ((prefix-commands
         (when exclude-prefix-commands
           (kmu-prefix-command-list))))
    (cl-loop for symbol being the symbols
             when (kmu-keymap-variable-p symbol)
             when (not (memq symbol prefix-commands))
             collect symbol)))

(defun kmu-prefix-command-list ()
  "Return a list of all prefix commands."
  (cl-loop for symbol being the symbols
           when (kmu-prefix-command-p symbol)
           collect symbol))

(defun kmu-read-mapvar (prompt)
  "Read the name of a keymap variable and return it as a symbol.
Prompt with PROMPT.  A keymap variable is one for which
`kmu-keymap-variable-p' returns non-nil."
  (let ((mapvar (intern (completing-read prompt obarray
                                         'kmu-keymap-variable-p t nil nil))))
    (if (eq mapvar '##)
        (error "No mapvar selected")
      mapvar)))

;;; Key Descriptions.

(defun kmu-key-description (keys)
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
            (kmu-key-description (vconcat (nreverse prefix))) " "))
         (kmu-key-description (vector (car keys))) ".."
         (kmu-key-description (vector (cdr keys)))))
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

;;; Defining Bindings.

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

;;; Keymap Mapping.

(defvar kmu-char-range-minimum 9)

(defun kmu-keymap-events (keymap &optional internal)
  (let ((min (1- kmu-char-range-minimum))
        v vv)
    (map-keymap-internal
     (lambda (key def)
       (if (kmu-keymap-list-p def)
           (setq v (append (kmu-keymap-events def key) v))
         (push (list key def) v)))
     keymap)
    (while v
      (let* ((elt (pop v))
             (def (cadr elt))
             beg end mem)
        (if (consp (car elt))
            (setq beg (caar elt)
                  end (cdar elt))
          (let (mem)
            (setq beg (car elt)
                  end (car elt))
            (while (and (setq mem (car (cl-member (1- beg) v :key 'car)))
                        (equal (cadr mem) def))
              (decf beg)
              (setq v (remove mem v)))
            (while (and (setq mem (car (cl-member (1+ end) v :key 'car)))
                        (equal (cadr mem) def))
              (incf end)
              (setq v (remove mem v)))))
        (cond ((eq beg end)
               (push elt vv))
              ((< (- end beg) min)
               (cl-loop for key from beg to end
                        do (push (list key def) vv)))
              (t
               (push (list (cons beg end) def) vv)))))
    (mapcar (if internal
                (lambda (elt)
                  (list (vconcat internal (list (car elt))) (cadr elt)))
              (lambda (elt)
                (list (vector (car elt)) (cadr elt))))
            vv)))

(defun kmu-map-keymap (function keymap)
  "Call FUNCTION once for each event sequence binding in KEYMAP.
FUNCTION is called with two arguments: the event sequence that is
bound (a vector), and the definition it is bound to.

When the definition of an event is another keymap list then
recursively build up a event sequence and instead of calling
FUNCTION with the initial event and it's definition once, call
FUNCTION once for each event sequence and the definition it is
bound to .

The last event in an event sequence may be a character range."
  (mapc (lambda (e) (apply function e)) (kmu-keymap-events keymap)))

(defun kmu-keymap-bindings (keymap)
  (let (bs)
    (kmu-map-keymap (lambda (key def)
                      (let ((a (assq def bs)))
                        (if a (setcdr a (cons key (cdr a)))
                          (push (list def key) bs))))
                    keymap)
    bs))

(defun kmu-map-keymap-bindings (function keymap)
  (mapc (lambda (e) (apply function e)) (kmu-keymap-bindings keymap)))

;;; `kmu-save-vanilla-keymaps-mode'.

(defvar kmu-save-vanilla-keymaps-mode-lighter " vanilla")

(define-minor-mode kmu-save-vanilla-keymaps-mode
  "Minor mode for saving vanilla keymaps.

When this mode is turned on a copy of the values of all loaded
keymap variables are saved.  While the mode is on all keymap
variables that haven't been saved yet are saved whenever a new
library is loaded.

This mode is useful when you want to compare the vanilla bindings
with your modifications.  To make sure you really get the vanilla
bindings turn on this mode as early as possible."
  :global t
  :keymap nil
  :lighter kmu-vanilla-keymap-mode-lighter
  (if kmu-save-vanilla-keymaps-mode
      (progn
        (kmu-save-vanilla-keymaps)
        (add-hook 'after-load-functions 'kmu-save-vanilla-keymaps))
    (remove-hook  'after-load-functions 'kmu-save-vanilla-keymaps)))

(defvar kmu-vanilla-keymaps nil)

(defun kmu-save-vanilla-keymaps (&optional filename)
  (interactive)
  (mapc 'kmu-save-vanilla-keymap (kmu-mapvar-list)))

(defun kmu-save-vanilla-keymap (mapvar)
  (interactive (list (kmu-read-mapvar "Save keymap: ")))
  (let ((e (assoc mapvar kmu-vanilla-keymaps)))
    (unless e
      (push (cons mapvar (copy-keymap (symbol-value mapvar)))
            kmu-vanilla-keymaps))))

(defun kmu-restore-vanilla-keymap (mapvar)
  (let ((vanilla (assoc mapvar kmu-vanilla-keymaps)))
    (if vanilla
        (setcdr (symbol-value mapvar)
                (cdr (copy-keymap vanilla)))
      (error "Vanilla state of %s hasn't been saved" mapvar))))

(defun kmu-vanilla-keymap (mapvar)
  (cdr (assq mapvar kmu-vanilla-keymaps)))

(defun kmu-vanilla-mapvar-p (mapvar)
  (equal (symbol-value mapvar)
         (assoc mapvar kmu-vanilla-keymaps)))

;;; Various.

(defun kmu-merge-esc-into-global-map ()
  (when (eq (lookup-key (current-global-map) [27]) 'ESC-prefix)
    (global-set-key [27] esc-map)))

(defun kmu-current-local-mapvar ()
  "Echo the variable bound to the current local keymap."
  (interactive)
  (let ((mapvar (kmu-keymap-variable (current-local-map))))
    (when (called-interactively-p 'any)
      (message (if mapvar
                   (symbol-name mapvar)
                 "Cannot determine current local keymap variable")))
    mapvar))

(provide 'keymap-utils)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; keymap-utils.el ends here
