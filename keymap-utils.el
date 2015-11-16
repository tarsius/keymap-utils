;;; keymap-utils.el --- keymap utilities

;; Copyright (C) 2008-2015  Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Package-Requires: ((cl-lib "0.3"))
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
(require 'dash)

;;; Predicates

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
The value returned is the keymap stored as OBJECT's variable
definition or else the variable which holds the keymap."
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

(defun kmu-menu-binding-p (object)
  "Return t if OBJECT is a menu binding."
  (and (listp object)
       (or (stringp (car object))
           (eq (car object) 'menu-item))))

;;; Key Lookup

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

;;; Keymap Variables

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

;;; Key Descriptions

(defun kmu-key-description (keys &optional prefix)
  "Return a pretty description of key-sequence KEYS.
Optional arg PREFIX is the sequence of keys leading up to KEYS.
For example, [?\C-x ?l] is converted into the string \"C-x l\".

For an approximate inverse of this, see `kmu-parse-key-description'."
  (let ((last (aref keys (1- (length keys)))))
    (if (and (consp last)
             (not (consp (cdr last))))
        ;; Handle character ranges.
        (progn
          (setq keys   (append keys nil)
                prefix (vconcat prefix (butlast keys))
                keys   (car (last keys)))
          (concat (and prefix (> (length prefix) 1)
                       (concat (kmu-key-description prefix) " "))
                  (kmu-key-description (vector (car keys))) ".."
                  (kmu-key-description (vector (cdr keys)))))
      (let ((s (replace-regexp-in-string "<\\([^>]+\\)>" "\\1"
                                         (key-description keys prefix) t)))
        ;; Merge ESC into following event.  FIXME is this still required?
        (while (and (string-match "\\(ESC \\([ACHsS]-\\)*\\([^ ]+\\)\\)" s)
                    (not (string-match-p "\\(ESC\\|M-\\)" (match-string 3 s))))
          (setq s (replace-match "\\2M-\\3" t nil s 1)))
        s))))

(defun kmu-parse-key-description (string &optional need-vector)
  (if (string-match "\\.\\." string)
      (cons (save-match-data
              (kmu-parse-key-description
               (substring string 0 (match-beginning 0)) need-vector))
            (save-match-data
              (kmu-parse-key-description
               (substring string (match-end 0)) need-vector)))
    (let ((case-fold-search nil)
          (len (length string)) ; We won't alter string in the loop below.
          (pos 0)
          (res []))
      (while (and (< pos len)
                  (string-match "[^ \t\n\f]+" string pos))
        (let* ((word-beg (match-beginning 0))
               (word-end (match-end 0))
               (word (substring string word-beg len))
               (times 1)
               key)
          ;; Try to catch events of the form "<as df>".
          (if (string-match "\\`<[^ <>\t\n\f][^>\t\n\f]*>" word)
              (setq word (match-string 0 word)
                    pos (+ word-beg (match-end 0)))
            (setq word (substring string word-beg word-end)
                  pos word-end))
          (when (string-match "\\([0-9]+\\)\\*." word)
            (setq times (string-to-number (substring word 0 (match-end 1))))
            (setq word (substring word (1+ (match-end 1)))))
          (cond ((string-match "^<<.+>>$" word)
                 (setq key (vconcat (if (eq (key-binding [?\M-x])
                                            'execute-extended-command)
                                        [?\M-x]
                                      (or (car (where-is-internal
                                                'execute-extended-command))
                                          [?\M-x]))
                                    (substring word 2 -2) "\r")))
                ((or (equal word "REM") (string-match "^;;" word))
                 (setq pos (string-match "$" string pos)))
                ((and (or (string-match "^\\(\\([ACHMsS]-\\)*\\)<\\(..+\\)>$" word)
                          (and (string-match
                                "^\\(\\([ACHMsS]-\\)*\\)\\([^ \t\f\n][^ \t\f\n]+\\)$"
                                word)
                               (not (string-match-p "\\([ACHMsS]-.\\)+$" word))))
                      (progn
                        (setq word (concat (substring word (match-beginning 1)
                                                      (match-end 1))
                                           (substring word (match-beginning 3)
                                                      (match-end 3))))
                        (not (string-match
                              "\\<\\(NUL\\|RET\\|LFD\\|ESC\\|SPC\\|DEL\\|TAB\\)$"
                              word))))
                 (setq key (list (intern word))))
                (t
                 (let ((orig-word word) (prefix 0) (bits 0))
                   (while (string-match "^[ACHMsS]-." word)
                     (cl-incf bits (cdr (assq (aref word 0)
                                              '((?A . ?\A-\^@) (?C . ?\C-\^@)
                                                (?H . ?\H-\^@) (?M . ?\M-\^@)
                                                (?s . ?\s-\^@) (?S . ?\S-\^@)))))
                     (cl-incf prefix 2)
                     (cl-callf substring word 2))
                   (when (string-match "^\\^.$" word)
                     (cl-incf bits ?\C-\^@)
                     (cl-incf prefix)
                     (cl-callf substring word 1))
                   (let ((found (assoc word '(("NUL" . "\0") ("RET" . "\r")
                                              ("LFD" . "\n") ("TAB" . "\t")
                                              ("ESC" . "\e") ("SPC" . " ")
                                              ("DEL" . "\177")))))
                     (when found (setq word (cdr found))))
                   (when (string-match "^\\\\[0-7]+$" word)
                     (cl-loop for ch across word
                              for n = 0 then (+ (* n 8) ch -48)
                              finally do (setq word (vector n))))
                   (cond ((= bits 0)
                          (setq key word))
                         ((and (= bits ?\M-\^@) (stringp word)
                               (string-match "^-?[0-9]+$" word))
                          (setq key (cl-loop for x across word
                                             collect (+ x bits))))
                         ((/= (length word) 1)
                          (error "%s must prefix a single character, not %s"
                                 (substring orig-word 0 prefix) word))
                         ((and (/= (logand bits ?\C-\^@) 0) (stringp word)
                               ;; We used to accept . and ? here,
                               ;; but . is simply wrong,
                               ;; and C-? is not used (we use DEL instead).
                               (string-match "[@-_a-z]" word))
                          (setq key (list (+ bits (- ?\C-\^@)
                                             (logand (aref word 0) 31)))))
                         (t
                          (setq key (list (+ bits (aref word 0)))))))))
          (when key
            (cl-loop repeat times do (cl-callf vconcat res key)))))
      (when (and (>= (length res) 4)
                 (eq (aref res 0) ?\C-x)
                 (eq (aref res 1) ?\()
                 (eq (aref res (- (length res) 2)) ?\C-x)
                 (eq (aref res (- (length res) 1)) ?\)))
        (setq res (cl-subseq res 2 -2)))
      (if (and (not need-vector)
               (cl-loop for ch across res
                        always (and (characterp ch)
                                    (let ((ch2 (logand ch (lognot ?\M-\^@))))
                                      (and (>= ch2 0) (<= ch2 127))))))
          (concat (cl-loop for ch across res
                           collect (if (= (logand ch ?\M-\^@) 0)
                                       ch (+ ch 128))))
        res))))

;;; Defining Bindings

(defun kmu-define-key (keymap key def)
  "In KEYMAP, define key sequence KEY as DEF.
This is like `define-key' but if KEY is a string then it has to
be a key description as returned by `key-description' and not a
string like \"?\C-a\"."
  (define-key keymap
    (if (stringp key) (kmu-parse-key-description key t) key)
    def))

(defun kmu-remove-key (keymap key)
  "In KEYMAP, remove key sequence KEY.
Make the event KEY truely undefined in KEYMAP by removing the
respective element of KEYMAP (or a sub-keymap) as opposed to
merely setting it's binding to nil.

There are several ways in which a key can be \"undefined\":

   (keymap (65 . undefined) ; A
           (66))            ; B

As far as key lookup is concerned A isn't undefined at all, it is
bound to the command `undefined' (which doesn't do anything but
make some noise).  This can be used to override lower-precedence
keymaps.

B's binding is nil which doesn't constitute a definition but does
take precedence over a default binding or a binding in the parent
keymap.  On the other hand, a binding of nil does _not_ override
lower-precedence keymaps; thus, if the local map gives a binding
of nil, Emacs uses the binding from the global map.

All other events are truly undefined in KEYMAP.

Note that in a full keymap all characters without modifiers are
always bound to something, the closest these events can get to
being undefined is being bound to nil like B above."
  (when (stringp key)
    (setq key (kmu-parse-key-description key t)))
  (define-key keymap key nil)
  (setq key (cl-mapcan (lambda (k)
                         (if (and (integerp k)
                                  (/= (logand k ?\M-\^@) 0))
                             (list ?\e (- k ?\M-\^@))
                           (list k)))
                       key))
  (if (= (length key) 1)
      (delete key keymap)
    (let* ((prefix (vconcat (butlast key)))
           (submap (lookup-key keymap prefix)))
      (delete (last key) submap)
      (when (= (length submap) 1)
        (kmu-remove-key keymap prefix)))))

(defmacro kmu-define-keys (mapvar feature &rest plist)
  "Define all keys in PLIST in the keymap stored in MAPVAR.

MAPVAR is a variable whose value is (or will be) a keymap.
FEATURE, if non-nil, is the feature provided by the library
that defines MAPVAR.  PLIST is a property list of the form
\(KEY DEF ...).

Each KEY is a either an event sequence vector or a string as
returned by `key-description'.  Each DEF can be anything that can
be a key's definition (see `define-key').  Additionally it can be
the keyword `:remove' in which case the existing definition (if
any) is removed from KEYMAP using `kmu-remove-key' (which see).

When FEATURE is nil MAPVAR's value is modified right away.
Otherwise it is modified immediately after FEATURE is loaded.
FEATURE may actually be a string, see `eval-after-load', though
normally it is a symbol.

Arguments aren't evaluated and therefore don't have to be quoted.
Also see `kmu-define-keys-1' which does evaluate it's arguments."
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
  "Define all keys in PLIST in the keymap KEYMAP.
KEYMAP may also be a variable whose value is a keymap.
Also see `kmu-define-keys'."
  (when (symbolp keymap)
    (setq keymap (symbol-value keymap)))
  (unless (keymapp keymap)
    (error "Not a keymap"))
  (while plist
    (unless (cdr plist)
      (error "Odd number of elements in PLIST"))
    (let ((key (pop plist))
          (def (pop plist)))
      (if (memq def '(:remove -> >))
          (kmu-remove-key keymap key)
        (kmu-define-key keymap key def)))))

;;; Keymap Mapping

(defvar kmu-char-range-minimum 9)

(defun kmu-keymap-bindings (keymap &optional prefix)
  "Return list of bindings in KEYMAP, traversing included keymaps recursively.
Each element of the returned list will be a list whose car is an event-sequence
and whose second element is the function/command it is bound to.
Prefix all key-sequences with PREFIX if supplied."
  (let ((min (1- kmu-char-range-minimum))
        v vv)
    (map-keymap-internal
     (lambda (key def)
       (if (kmu-keymap-list-p def)
           (setq v (append
                    (kmu-keymap-bindings def (list key))
                    v))
         (push (list key def) v)))
     keymap)
    (while v
      (let* ((elt (pop v))
             (key (car elt))
             (def (cadr elt))
             beg end mem)
        (if (vectorp key)
            (push elt vv)
          (if (consp key)
              (setq beg (car key) end (cdr key))
            (when (integerp key)
              (setq beg key end key)
              (while (and (setq mem (car (cl-member (1- beg) v :key 'car)))
                          (equal (cadr mem) def))
                (cl-decf beg)
                (setq v (remove mem v)))
              (while (and (setq mem (car (cl-member (1+ end) v :key 'car)))
                          (equal (cadr mem) def))
                (cl-incf end)
                (setq v (remove mem v)))))
          (cond ((or (not beg) (eq beg end))
                 (push (list key def) vv))
                ((< (- end beg) min)
                 (cl-loop for key from beg to end
                          do (push (list key def) vv)))
                (t
                 (push (list (cons beg end) def) vv))))))
    (mapcar (lambda (e)
              (let ((k (car e)))
                (list (vconcat prefix (if (vectorp k) k (vector k)))
                      (cadr e))))
            vv)))

(defun kmu-map-keymap (function keymap)
  "Call FUNCTION once for each event sequence binding in KEYMAP.
FUNCTION is called with two arguments: the event sequence that is
bound (a vector), and the definition it is bound to.

When the definition of an event is another keymap list then
recursively build up an event sequence and instead of calling
FUNCTION with the initial event and it's definition once, call
FUNCTION once for each event sequence and the definition it is
bound to.

The last event in an event sequence may be a character range."
  (mapc (lambda (e) (apply function e)) (kmu-keymap-bindings keymap)))

(defun kmu-keymap-definitions (keymap &optional nomenu nomouse)
  "Return a list of all definitions and corresponding keys in KEYMAP.
Any keymap definitions in KEYMAP will be traversed recursively.
Each element of the returned list will be a list whose car is a definition
and whose cdr is a list of keys bound to that definition.
If NOMENU is non-nil then skip menu items, and if NOMOUSE is non-nil
skip mouse events."
  (let (bs)
    (kmu-map-keymap (lambda (key def)
                      (cond ((and nomenu (kmu-menu-binding-p def)))
                            ((and nomouse (mouse-event-p (aref key 0))))
                            (t
                             (let ((a (assq def bs)))
                               (if a (setcdr a (cons key (cdr a)))
                                 (push (list def key) bs))))))
                    keymap)
    bs))

(defun kmu-map-keymap-definitions (function keymap &optional nomenu nomouse)
  "Call FUNCTION once for each different definition in KEYMAP.
FUNCTION will be called with 2 args: the key binding definition and the list of
all event sequences bound to that definition.
Any keymap definitions in KEYMAP will be traversed recursively as in `kmu-map-keymap'.
The optional NOMENU & NOMOUSE args are as for `kmu-map-keymap'."
  (mapc (lambda (e) (apply function e))
        (kmu-keymap-definitions keymap nomenu nomouse)))

;;; Vanilla Keymaps

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
  :lighter kmu-save-vanilla-keymaps-mode-lighter
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

;;; Various

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

(defun kmu-eval-keymap (keymap)
  "Return the keymap pointed to by KEYMAP, or KEYMAP itself if it is a keymap."
  (cond ((kmu-keymap-variable-p keymap) (eval keymap))
        ((keymapp keymap) keymap)))

(defun kmu-keymaps-in-file (file &optional eval)
  "Return a list of keymaps and variables pointing to keymaps that are used in FILE.
If EVAL is non-nil eval any variables in the returned list."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let ((sexps
           (cl-loop with it
                    while (setq it (condition-case v
                                       (read (current-buffer)) (error nil)))
                    collect it)))
      (cl-remove-if-not
       (lambda (x) (or (keymapp x)
                       (kmu-keymap-variable-p x)))
       (cl-remove-duplicates (-flatten sexps))))))

(defun kmu-command-key-description (cmd &optional keymaps sep)
  "Return description of key-sequence for CMD.
The KEYMAPS can be a single keymap/variable or list of keymaps/variables to search for CMD,
otherwise `overriding-local-map' is searched.
By default the first keybinding will be returned, but if SEP is supplied it will be used
to seperate the descriptions of all key-sequences bound to CMD.
If there is no key-sequence for command then a string in the form \"M-x CMD\" will be returned."
  (let* ((keymaps2 (if keymaps
		       (if (listp keymaps)
			   ;; Note: don't bother trying to put this function in
			   ;; a cl-flet form, or you won't be able to do the mapcar
			   (mapcar 'kmu-eval-keymap keymaps)
			 (kmu-eval-keymap keymaps))))
	 (key (where-is-internal cmd (or keymaps2 overriding-local-map) (unless sep t))))
    (if key
        (if sep
            (mapconcat 'key-description key sep)
          (key-description key))
      (format "M-x %s" cmd))))

(provide 'keymap-utils)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; keymap-utils.el ends here
