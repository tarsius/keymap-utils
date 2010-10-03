;;; keymap-utils.el --- keymap utilities

;; Copyright (C) 2008, 2009, 2010  Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Created: 20080830
;; Updated: 20101101
;; Version: 0.3.0
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
;; * replace keymap in place (e.g. `kmu-set-mapvar')
;; * keymap pretty printing (`kmu-pp-keymap'; VERY experimental)

;;; Code:

(require 'cl)
(require 'save-sexp)

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
  "Return a copy of KEYMAP with all parent keymaps removed."
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

(defun kmu-keymap-variable (--keymap-- &rest exclude)
  "Return a symbol whose value is KEYMAP.

Comparison is done with `eq'.  If there are multiple variables
whose value is KEYMAP it is undefined which is returned.

Ignore symbols listed in optional EXCLUDE.  Use this to prevent
symbols from being returned which are dynamically bound to KEYMAP."
  (setq exclude (append '(--keymap-- --match-- --symbol--) exclude))
  (let (--match--)
    (do-symbols (--symbol--)
      (and (not (memq --symbol-- exclude))
	   (boundp --symbol--)
	   (eq (symbol-value --symbol--) --keymap--)
	   (setq --match-- --symbol--)
	   (return nil)))
    --match--))

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

(defun kmu-set-mapvar* (variable keymap)
  "Set the cdr of the default value of VARIABLE to the cdr of KEYMAP.
Both VARIABLE and KEYMAP are evaluated.  Also see `kmu-set-mapvar'."
  (cond ((kmu-keymap-variable-p variable)
	 (setcdr (default-value variable) (cdr keymap)))
	((or (not (boundp variable))
	     (not (default-value variable)))
	 (set-default variable (cons 'keymap (cdr keymap))))
	(t
	 (error "Can't set variable, it's value isn't a keymap: %s"
		variable))))

(defmacro kmu-set-mapvar (variable eval-after-load keymap)
  "Set the cdr of the default value of VARIABLE to the cdr of KEYMAP.
VARIABLE isn't evaluated but KEYMAP is.

This macro expands to a call to `kmu-set-mapvar*' (which see).
If EVAL-AFTER-LOAD is non-nil this is additionally wrapped in an
`eval-after-load' form (which see) with EVAL-AFTER-LOAD as the FILE
argument."
  (declare (indent 2) (debug t))
  (if eval-after-load
      `(eval-after-load ,eval-after-load
	 (kmu-set-mapvar* ',variable ,keymap))
    `(kmu-set-mapvar* ',variable ,keymap)))

;;; Collections.

(defun kmu-map-keymap (function keymap &optional prefix)
  "Call FUNCTION once for each event sequence binding in KEYMAP.
FUNCTION is called with two arguments: an event sequence (a vector), and
the definition the last event in that sequence it is bound to.  Each event
may also be a character range.

When the definition an event is bound to is a prefix key but not a prefix
command then instead of calling FUNCTION with the event and it's definition
once, FUNCTION is called for each event binding in the sub-keymap.  This is
done recursively until reaching an event binding that is not a prefix, in
each branch.  FUNCTION is called with the sequence that lead to the event
binding, relative to KEYMAP, as first argument and the final binding as
second argument.

If KEYMAP has a parent, this function returns it without processing it.

\(fn FUNCTION KEYMAP)" ; PREFIX is used internally
  (map-keymap-internal
   (lambda (type bind)
     (if (kmu-keymap-list-p bind)
	 (kmu-map-keymap function bind
			 (vconcat prefix (list type)))
       (unless (consp type) ;; tempory
	 (funcall function (vconcat prefix (list type)) bind))))
   keymap))

(defun kmu-key-argument (type)
  (let (has-symbol-p)
    (dotimes (i (length type))
      (when (symbolp (aref type i))
	(setq has-symbol-p t)))
    (if has-symbol-p type (key-description type))))

(defun kmu-keymap-to-keytree (keymap groups)
  "Return a tree of event bindings in KEYMAP according to GROUPS.

GROUPS is a list of event groups; each event group is a list whose car is
the title of the group and whose cdr is a list of subgroups..."
  (let* ((bindings (let (bs)
		     (kmu-map-keymap
		      (lambda (type bind)
			(push (cons (kmu-key-argument type) bind) bs))
		      keymap)
		     (reverse bs)))
	 (ranges   (mapcan
		    (lambda (elt)
		      (when (consp (car elt))
			(setq bindings (delete elt bindings))
			(list elt)))
		    bindings))
	 (tree     (mapcan
		    (lambda (group)
		      (let ((desc (kmu--separate-group group bindings)))
			(setq bindings (cdr desc))
			(when (car desc)
			  (list (cons (car group) (car desc))))))
		    groups)))
    (nconc (list (keymap-prompt keymap)
		 (kmu-full-keymap-p keymap)
		 (kmu-keymap-parent keymap)
		 (let ((default (or (assoc "<t>" bindings)
				    (assoc  [t]  bindings))))
		   (when default
		     (delete default bindings)
		     (cdr default))))
	   (nconc (when ranges
		    (list (cons kmu-range-group-pp-title
				(list (sort* ranges '< :key 'caar)))))
		  tree
		  (when bindings
		    (list (cons kmu-implicit-group-pp-title
				;;(list (sort* bindings '< :key 'caar))
				(list bindings)
				)))))))

(defun kmu--separate-group (group bindings)
  (cons (mapcan (lambda (subgroup)
		  (let ((desc (kmu--separate-subgroup subgroup bindings)))
		    (setq bindings (cdr desc))
		    (when (car desc)
		      (list (car desc)))))
		(cdr group))
	bindings))

(defun kmu--separate-subgroup (subgroup bindings)
  (cons (if (stringp subgroup)
	    (mapcan (lambda (elt)
		      (when (and (vectorp (car elt))
				 (symbolp (aref (car elt) 0))
				 (string-match subgroup
					       (symbol-name
						(aref (car elt) 0))))
			(setq bindings (delete elt bindings))
			(list elt)))
		    bindings)
	  (mapcan (lambda (elt)
		    (let ((elt (assoc elt bindings)))
		      (when elt
			(setq bindings (delete elt bindings))
			(list elt))))
		  subgroup))
	bindings))

;;; Pretty Printing.

(defvar kmu-range-group-pp-title nil
  "The title used for the range group used for pretty-printing.")

(defvar kmu-implicit-group-pp-title "others"
  "The title used for the implicit group used for pretty-printing.")

(defun kmu-save-mapvar-setq (mapvar file)
  (save-sexp-save mapvar file 'save-sexp-save-setq-1
		  (lambda (_)
		    (kmu-pp-keymap (bound-and-true-p variable)
				   (bound-and-true-p default-groups)
				   nil nil))))

(defun kmu-save-mapvar-set-mapvar (mapvar file &optional eval-after-load)
  ;; TODO support docstring and various keywords:
  ;; :type (same sparse full), :reset-load-file,
  (save-sexp-save
   mapvar file
   (lambda (variable pp)
     (save-sexp-save-helper 'kmu-set-mapvar variable
       (princ (format "(kmu-set-mapvar %s %s" variable eval-after-load))
       (cond (pp (princ "\n")
		 (princ (save-sexp-pp-indent (funcall pp value) 2)))
	     (t  (princ " ")
		 (prin1 value)))
       (when (looking-back "\n")
	 (delete-char -1))
       (princ ")")))
   (lambda (_)
     ;; FIXME do not depend on dynamic bindings
     (kmu-pp-keymap (bound-and-true-p variable)
		    (bound-and-true-p default-groups)
		    (bound-and-true-p kmu-align)
		    nil))))

(defun kmu-pp-keymap (keymap groups &optional align describe)
  "Return the pretty-printed representation of KEYMAP.

KEYMAP must be a keymap or a variable whose value is a keymap.  If the
keymap is held by a global variable (as opposed to some local variable) it
is strongly recommended to pass the symbol not the keymap to this function
as this helps in some special cases (which are not to uncommon).

The returned string looks similar to what a human would write to create a
keymap.  Its the pretty-printed representation of a `let' form which binds
`map' as a full or sparse keymap, contains `define-key' forms for all
event bindings in the keymap and recursively it's submaps, and finally
returns `map'.

The order in which bindings in the keymap are pretty-printed can be
controlled using GROUPS; see `kmu-keymap-to-keytree'.

Events that are not part of any group in GROUPS are part of a final
implicit group whose title is set using `kmu-implicit-group-pp-title'.

If optional ALIGN is non-nil the bindings in the `define-key' forms are
aligned accordingly.  It should be an integer `keymap', `group' or
`subgroup'.

Optional DESCRIBE if non-nil should be a function that can convert an
event vector into something suitable for pretty-printing.  If it is not
defined `kmu-event-description' is used."
  (let ((tree (kmu-keymap-to-keytree
	       (if (symbolp keymap)
		   (symbol-value keymap)
		 keymap)
	       groups
	       ;; ??? (or describe 'kmu-event-description)
	       )))
    (concat (format "(let ((map (make%s-keymap%s)))\n"
		    (if (cadr tree) "" "-sparse")
		    (if (car tree) (format " %S" (car tree)) ""))
	    (let ((parmap (caddr tree)))
	      (when parmap
		(format "  (set-keymap-parent map %s)\n"
			(if (symbolp parmap)
			    parmap
			  (concat "\n"
				  (kmu--indent-string
				   (kmu-pp-keymap parmap groups) 4))))))
	    "\n"
	    (when (car (cdddr tree))
	      (format "  %s\n\n"
		      (kmu-pp-key-binding [t] (car (cdddr tree)))))
	    (let ((align (kmu--align-keymap groups align))
		  (mapvar (when (symbolp keymap) keymap)))
	      (mapconcat (lambda (group)
			   (kmu--pp-keygroup group align mapvar groups))
			 (nthcdr 4 tree) "\n"))
	    "\n  map)")))

(defun kmu--pp-keygroup (group &optional align mapvar groups)
  (setq align (kmu--align-group group align))
  (concat (when (car group)
	    (format "  ;; %s\n\n" (car group)))
	  (mapconcat (lambda (subgroup)
		       (kmu--pp-bindings subgroup align mapvar groups))
		     (cdr group) "\n")))

(defun kmu--pp-bindings (bindings &optional align mapvar groups)
  (setq align (kum--align-subgroup bindings align))
  (concat (mapconcat
	   (lambda (elt)
	     (kmu--indent-string
	      (cond ((consp (car elt))
		     (kmu-pp-char-range (car elt) (cdr elt)))
		    (t
		     (kmu-pp-key-binding (car elt) (cdr elt)
					 align mapvar groups)))
	      2))
	   bindings "\n")
	  "\n"))

(defun kmu-pp-key-binding (key binding &optional align mapvar groups)
  (format "(define-key map %s)"
	  (kmu--pp-binding key binding align mapvar groups)))

(defun kmu-pp-char-range (range binding)
  (format (concat "(let ((c %s))\n"
		  "  (while (<= c %s)\n"
		  "    %s\n"
		  "    (incf c)))\n")
	  (car range) (cdr range)
	  (kmu-pp-key-binding '(vector c) binding)))

(defun kmu--pp-binding (event binding &optional align mapvar groups)
  (concat (kmu--pp-event event align)
	  (cond ((not binding)
		 " nil")
		((symbolp binding)
		 (format " '%s" binding))
		((kmu-keymap-list-p binding)
		 (let ((var (kmu-keymap-variable binding 'binding)))
		   (if var
		       (format " '%s" var)
		     (concat "\n"
			     (kmu--indent-string
			      (kmu-pp-keymap binding groups align)
			      4)))))
		(t
		 ;; FIXME handle menus properly
		 (if mapvar
		     (format " (lookup-key %s %S)" mapvar event)
		   (error "PP not implemented (%s) for form: %s"
			  "pass MAPVAR for a workaround" binding))))))

(defun kmu--pp-event* (event)
  (prin1-to-string (if (stringp event)
		       (list 'kbd event)
		     event)))

(defun kmu--pp-event (event &optional align)
  (let* ((str (kmu--pp-event* event))
	 (len (length str)))
    (if (or (not align) (>= len align))
	str
      (concat str (make-string (- align len) ?\s)))))

(defun kmu--indent-string (string indent)
  (replace-regexp-in-string
   "^\\([\s\t]+\\|\\)[^\n]"
   (lambda (str)
     (save-match-data
       (string-match "\\(\t+\\)?\\(\s+\\)?" str)
       (let ((len (+ (* 8 (length (match-string 1 str)))
		     (length (match-string 2 str))
		     indent)))
	 (concat (make-string (/ len 8) ?\t)
		 (make-string (% len 8) ?\s)))))
   string nil nil 1))


(defun kmu--event-string-length (event)
  (length (kmu--pp-event* (if (consp event) (car event) event))))

(defun kmu--align-keymap (groups align)
  (if (eq align 'keymap)
      (apply 'max (mapcan (lambda (group)
			    (mapcan
			     (lambda (subgroup)
			       (mapcar 'kmu--event-string-length subgroup))
			     (cdr group)))
		   groups))
    align))

(defun kmu--align-group (group align)
  (if (eq align 'group)
      (apply 'max (mapcan (lambda (subgroup)
			    (mapcar 'kmu--event-string-length subgroup))
			  (cdr group)))
    align))

(defun kum--align-subgroup (bindings align)
  (if (eq align 'subgroup)
      (apply 'max (mapcar 'kmu--event-string-length bindings))
    align))

(provide 'keymap-utils)
;;; keymap-utils.el ends here
