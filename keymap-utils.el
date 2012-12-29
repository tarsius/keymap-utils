;;; keymap-utils.el --- keymap utilities

;; Copyright (C) 2008-2012  Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Created: 20080830
;; Version: 0.4.4
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

(require 'cl-lib) ; cl-loop, cl-labels

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

;;; Keymap Mapping.

(defun kmu-map-keymap (function keymap &optional internal)
  "Call FUNCTION once for each event sequence binding in KEYMAP.
FUNCTION is called with two arguments: an event sequence (a
vector), and the definition the last event in that sequence it
is bound to.

When an event's definition is another keymap (for which
`kmu-keymap-list-p' returns non-nil) then recursively build up a
event sequence and instead of calling FUNCTION with the initial
event and it's definition once, call FUNCTION with each event
sequence.

If the last event in an event sequence is actually a character
range then call FUNCTION with a dotted list instead of a vector
as event sequence argument.

\(fn FUNCTION KEYMAP)"
  (map-keymap-internal
   (lambda (key def)
     (setq key (if (consp key)
                   (append internal key)
                 (vconcat internal (list key))))
     (if (kmu-keymap-list-p def)
         (kmu-map-keymap function def key)
       (funcall function key def)))
   keymap)
  nil)

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
