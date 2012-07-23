;;; keymap-utils.el --- keymap utilities

;; Copyright (C) 2008-2012  Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Created: 20080830
;; Version: 0.4.0-git
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

;; This library provides additional functions to work with keymaps.
;;
;; * keymap predicates (e.g. `kmu-keymap-variable-p')
;; * key lookup (e.g. `kmu-lookup-parent-key')
;; * and more

;;; Code:

(require 'cl)
(require 'edmacro)

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
  (flet ((strip-keymap (keymap)
	   (set-keymap-parent keymap nil)
	   (loop for key being the key-code of keymap
		 using (key-binding binding) do
		 (and (keymapp binding)
		      (not (kmu-prefix-command-p binding))
		      (strip-keymap binding)))
	   keymap))
    (strip-keymap (copy-keymap keymap))))

(defun kmu--collect-parmaps (keymap)
  "Return a copy of KEYMAP with all local bindings removed."
  (flet ((collect-parmaps (keymap)
	   (let ((new-keymap (make-sparse-keymap)))
	     (set-keymap-parent new-keymap (keymap-parent keymap))
	     (set-keymap-parent keymap nil)
	     (loop for key being the key-code of keymap
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
      (do-symbols (--symbol--)
	(and (not (memq --symbol-- exclude))
	     (boundp --symbol--)
	     (eq (symbol-value --symbol--) keymap)
	     (setq --match-- --symbol--)
	     (return nil)))
      --match--)))

(defun kmu-keymap-parent (keymap &optional need-symbol &rest exclude)
  "Return the parent keymap of KEYMAP.

If a variable exists whose value is KEYMAP's parent keymap return that.
Otherwise if KEYMAP does not have a parent keymap return nil.  Otherwise
if KEYMAP has a parent keymap but no variable is bound to it return the
parent keymap, unless optional NEED-SYMBOL is non-nil in which case nil
is returned.

Also see `kmu-keymap-variable'."
  (let ((--parmap-- (keymap-parent keymap)))
    (when --parmap--
      (or (kmu-keymap-variable --parmap-- '--parmap--)
	  (unless need-symbol --parmap--)))))

(defun kmu-mapvar-list (&optional exclude-prefix-commands)
  "Return a list of all keymap variables.

If optional EXCLUDE-PREFIX-COMMANDS is non-nil exclude all variables
whose variable definition is also the function definition of a prefix
command."
  (let ((prefix-commands
	 (when exclude-prefix-commands
	   (kmu-prefix-command-list))))
    (loop for symbol being the symbols
	  when (kmu-keymap-variable-p symbol)
	  when (not (memq symbol prefix-commands))
	  collect symbol)))

(defun kmu-prefix-command-list ()
  "Return a list of all prefix commands."
  (loop for symbol being the symbols
	when (kmu-prefix-command-p symbol)
	collect symbol))

(defun kmu-read-mapvar (prompt)
  (let ((mapvar (intern (completing-read prompt obarray
					 'kmu-keymap-variable-p t nil nil))))
    (if (eq mapvar '##)
	(error "No mapvar selected")
      mapvar)))

;;; Keymap Mapping.

(defun kmu-map-keymap (function keymap &optional pretty prefix)
  "Call FUNCTION once for each event sequence binding in KEYMAP.
FUNCTION is called with two arguments: an event sequence (a vector), and
the definition the last event in that sequence it is bound to.  Each event
may also be a character range.

When the definition an event is bound to is a prefix key but not a prefix
command then instead of calling FUNCTION with the event and it's definition
once, FUNCTION is called for each event binding in the sub-keymap.  This is
done recursively until reaching an event binding that is not a prefix, in
each branch.  FUNCTION is called with the sequence that leads to the event
binding, relative to KEYMAP, as first argument and the final binding as
second argument.

If KEYMAP has a parent, this function returns it without processing it.
Optional PREFIX is used internally to do this; do not set it yourself.

If optional PRETTY is non-nil call FUNCTION with a string suitable for
`kbd' instead of a vector as first argument (provided it's not a
character range)."
  (map-keymap-internal
   (lambda (key def)
     (let ((vec (vconcat prefix (list key))))
       (cond
	((kmu-keymap-list-p def) (kmu-map-keymap function def pretty vec))
	((eq def 'ESC-prefix)    (kmu-map-keymap function esc-map pretty vec))
	((consp key)             (funcall function key def))
	(pretty                  (funcall function (key-description vec) def))
	(t                       (funcall function key def)))))
   keymap))

;;; Defining Bindings.

(defun kmu-undefine-key (keymap key)
  (define-key keymap key nil)
  (delete (cons key nil) keymap))

(defmacro kmu-define-keys (keymap &rest plist)
  "Define all keys in PLIST in KEYMAP."
  (declare (indent 1))
  `(kmu-define-keys-1 ',keymap ',plist))

(defun kmu-define-keys-1 (keymap plist &optional safe)
  (cond ((null keymap)
	 (error "Can't set keys in a null keymap"))
	((symbolp keymap)
	 (setq keymap (symbol-value keymap)))
	((keymapp keymap))
	((listp keymap)
	 (unless (kmu-prefix-command-p (caddr keymap))
	   (set (caddr keymap) nil)
	   (define-prefix-command (caddr keymap) (cadddr keymap)))
	 (define-key (symbol-value (car keymap)) (cadr keymap) (caddr keymap))
	 (setq keymap (symbol-function (caddr keymap)))))
  (let (key)
    (while plist
      (etypecase (car plist)
	(keyword (case (car plist)
		   (:undefine (mapc (apply-partially 'kmu-undefine-key keymap)
				    (cadr plist)))
		   )
		 (setq key nil plist (cddr plist)))
	(string  (setq key (pop plist)))
	(symbol  (setq key (symbol-value (pop plist))))
	(cons    (setq key (vector (caar plist)))
		 (when (= (1+ (caar plist)) (cdar plist))
		   (setq plist (cdr plist)))))
      (when key
	(setq key (edmacro-parse-keys key))
	(if (or (not safe)
		(eq (lookup-key keymap key) 'undefined))
	    (define-key keymap key (pop plist))
	  (pop plist))))))

(defmacro kmu-restore-global-bindings (mapvar &rest events)
  (declare (indent 1))
  `(let ((map ,mapvar))
     (dolist (e '(,@events))
       (setq  e (edmacro-parse-keys e))
       (define-key map e (lookup-key (current-global-map) e)))))

(provide 'keymap-utils)
;;; keymap-utils.el ends here
