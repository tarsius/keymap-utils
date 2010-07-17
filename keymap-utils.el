;;; keymap-utils.el --- keymap utilities

;; Copyright (C) 2008, 2009, 2010  Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Created: 20080830
;; Updated: 20100713
;; Version: 0.3_pre
;; Homepage: http://github.com/tarsius/keymap-utils
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

;; This library provides additional keymap predicates and other keymap
;; functions.

;; This library assumes that the char-table of a full keymap is to be
;; found in its cadr.  This actually does not have to be the case but
;; it is for keymaps that have been created using the standart mechanism
;; for creating and modifying keymaps.  Key lookup in a full keymap that
;; has other bindings before the char-table still works as expected, but
;; since it is a common assumption that the char-table is in the cadr
;; (the doc-string of `make-keymap' states that full keymaps have the
;; form (keymap CHARTABLE . ALIST) ), it is legimate that I make this
;; assumtion here also.  If your library has problems because of this
;; then it probably should be fixed; not `keymap-utils'.

;; This library is not intended to be used with menus, before using a
;; function with a menu make sure it still behaves as you intended.

;;; Code:

(require 'cl) ; copy-list, mapcan

;;; Redefining Keymaps.

(defun kmu-set-mapvar* (variable keymap)
  "Set the cdr of the default value of VARIABLE to the cdr of KEYMAP.
Both VARIABLE and KEYMAP are evaluated.  Also see `kmu-set-mapvar'."
  (let ((tail (car (last keymap))))
    (cond ((mapvarp variable)
	   (setcdr (default-value variable) (cdr keymap)))
	  ((or (not (boundp variable))
	       (not (default-value variable)))
	   (set-default variable (cons 'keymap (cdr keymap))))
	  (t
	   (error "Can't set keymap variable: %s" variable)))))

(defmacro kmu-set-mapvar (variable keymap)
  "Set the cdr of the default value of VARIABLE to the cdr of KEYMAP.
VARIABLE isn't evaluated but KEYMAP is.  Also see `kmu-set-mapvar*'."
  (declare (indent 1))
  `(kmu-set-mapvar* ',variable ,keymap))

;;; Keymap Predicates.

(defun kmu-keymap-variable-p (object)
  "Return t if OBJECT is a symbol whose variable definition is a keymap."
  (and (symbolp object)
       (boundp  object)
       (keymapp (symbol-value object))))

(defun kmu-keymap-list-p (object)
  "Return t if OBJECT is a list whose first element is the symbol `keymap'."
  (and (listp   object)
       (keymapp object)))

(defun kmu-prefix-command-p (object)
  "Return t if OBJECT is a symbol whose function definition is a keymap."
  (and (symbolp object)
       (fboundp object)
       (keymapp (symbol-function object))))

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

(defun kmu-strip-keymap (keymap)
  (flet ((strip-keymap (keymap)
	   (set-keymap-parent keymap nil)
	   (loop for key being the key-code of keymap
		 using (key-binding binding) do
		 (and (keymapp binding)
		      (not (kmu-prefix-command-p binding))
		      (strip-keymap binding)))
	   keymap))
    (strip-keymap (copy-keymap keymap))))

(defun kmu-collect-parmaps (keymap)
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

(defun kmu-lookup-local-key (keymap key &optional accept-default)
  "In keymap KEYMAP, look up key sequence KEY.  Return the definition.

Unlike `lookup-key' (which see) this doesn't consider bindings made
in KEYMAP's parent keymap."
  (lookup-key (kmu-strip-keymap keymap) key accept-default))

(defun kmu-lookup-parent-key (keymap key &optional accept-default)
  "In keymap KEYMAP's parent keymap, look up key sequence KEY.

Return the definition.  Unlike `lookup-key' (which see) this only
conciders bindings made in KEYMAP's parent keymap and recursivly
all parent keymaps of local keymaps that keys are bound to."
  (lookup-key (kmu-collect-parmaps keymap) key accept-default))

;;; Keymap details.

(defun kmu-keymap-event-bindings (keymap &optional prefix)
  "Return alist of all bindings in keymap KEYMAP.

The car is the event and the cdr the command bound to it.  If PREFIX is
non-nil, it is a vector of input events leading up to the event and is
included in the the car of the respective entry."
  (let (bindings)
    (map-keymap-internal (lambda (type def)
			   (if (and (eq type 27)
				    (kmu-keymap-list-p def))
			       (setq bindings
				     (nconc (kmu-keymap-event-bindings
					     def (vector 27))
					    bindings))
			     (push (cons (if (consp type)
					     (copy-list type)
					   (vconcat prefix (list type)))
					 def)
				   bindings)))
			 keymap)
    (mapcan (lambda (elt)
	      (if (consp (car elt))
		  (loop for i from (caar elt) to (cdar elt)
			collect (cons (vector i) (cdr elt)))
		(list elt)))
	      bindings)))

(defun kmu-keymap-parent (keymap)
  "Return the parent keymap of KEYMAP.

If a variable can be found that holds the parent keymap return that
symbol.  Otherwise return the parent keymap itself, or if keymap has
no parent keymap return nil.  If there is more than one variable
holding the parent keymap it is unpredictable which is returned."
  (let ((parmap (keymap-parent keymap)))
    (when parmap
      (or (do-symbols (symbol)
	    (and (not (memq symbol '(symbol parmap)))
		 (boundp symbol)
		 (eq (symbol-value symbol) parmap)
		 (return symbol)))
	  parmap))))

;;; Converters.

(defun kmu-convert-full-keymap-to-sparse (keymap)
  "Convert full keymap KEYMAP to a sparse keymap and return it.
The original keymap is not altered."
  (unless (kmu-full-keymap-p keymap)
    (error "Not a full keymap."))
  (let ((new-map (make-sparse-keymap)))
    (set-keymap-parent new-map (keymap-parent keymap))
    (loop for event being the key-codes of keymap
	  using (key-bindings binding) do
	  (define-key new-map event binding))
    new-map))

(defun kmu-nconvert-full-keymap-to-sparse (keymap)
  "Convert full keymap KEYMAP to a sparse keymap and return it.
The original keymap _is_ altered."
  (unless (kmu-full-keymap-p keymap)
    (error "Not a full keymap."))
  (let ((temp-map (cons 'keymap (list (cadr keymap)))))
    (setcdr keymap (keymap-parent keymap))
    (set-keymap-parent keymap (keymap-parent temp-map))
    (loop for event being the key-codes of temp-map
	  using (key-bindings binding) do
	  (define-key keymap event binding))
    keymap))

(defun kmu-convert-sparse-keymap-to-full (keymap)
  "Convert sparse keymap KEYMAP to a full keymap and return it.
The original keymap is not altered."
  (unless (kmu-sparse-keymap-p keymap)
    (error "Not a sparse keymap."))
  (let ((new-map (make-keymap)))
    (set-keymap-parent new-map (keymap-parent keymap))
    (loop for event being the key-codes of keymap
	  using (key-bindings binding) do
	  (define-key new-map event binding))
    new-map))

(defun kmu-nconvert-sparse-keymap-to-full (keymap)
  "Convert sparse keymap KEYMAP to a full keymap and return it.
The original keymap _is_ altered."
  (unless (kmu-sparse-keymap-p keymap)
    (error "Not a sparse keymap."))
  (let ((temp-map (copy-keymap keymap)))
    (setcdr keymap (keymap-parent keymap))
    (loop for event being the key-codes of temp-map
	  using (key-bindings binding) do
	  (define-key keymap event binding))
    keymap))

(provide 'keymap-utils)
;;; keymap-utils.el ends here
