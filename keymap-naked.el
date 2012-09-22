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

(require 'cl)
(require 'naked)

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
      (delete (last (setq key (append key nil)))
              (lookup-key keymap (apply 'vector (butlast key))))
    (delete (cons key nil) keymap)))

(defmacro kmu-define-keys (mapvar feature &rest plist)
  "Define all keys in PLIST in the keymap stored in MAPVAR.
If a key in PLIST is a string it has to be in the `naked' format
without angle brackets.  `:remove' as a key definition means that
the existing definition (if any) should be remove from the keymap
using `kmu-remove-key'."
  (declare (indent 2))
  (if feature
      `(eval-after-load ',feature
         (kmu-define-keys-1 ',mapvar ',plist))
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

(defun kmu-naked-key-description (keys)
  "Like `naked-key-description' but also handle some special cases."
  (flet ((describe
          (keys)
          ;; "Quote" certain events that cannot be encoded.
          (case (aref keys 0)
            (128     "128")
            (4194303 "255")
            (t       (naked-key-description keys)))))
    (if (consp keys)
        ;; A string representation for character ranges.
        (let (prefix)
          (while (consp (cdr keys))
            (push (car keys) prefix)
            (setq keys (cdr keys)))
          (concat
           (when prefix
             (concat
              (describe (vconcat (nreverse prefix))) " "))
           (describe (vector (car keys))) ".."
           (describe (vector (cdr keys)))))
      (describe keys))))

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

(defun kmu-naked-keymap-bindings
  (keymap &optional order separate exclude)
  "Return a list of event sequence binding in KEYMAP.
Each element has the form (DEF EVENT...) where DEF is the
definition and the cdr is a list of (in most cases) all
event sequences bound to DEF.

Each EVENT is an event sequence as returned by
`naked-key-description' and might end with an event range
represented as \"START .. END\".  Possible ranges are e.g.
\"1 .. 9\", \"M-0 .. M-9\", \"C-c C-0 .. C-9\" and actual
character (integer) ranges.

If optional ORDER is non-nil both the returned list and it's
elements are sorted accordingly.  ORDER should be a list of
naked event strings, or a list of such lists; events in the
return value are brought into the same order.

Optional EXCLUDE and SEPARATE specify events which should
not be part of the returned value respectively as separate
elements.  If non-nil the car of both EXCLUDE and SEPARATE
have to regular expressions or nil; the cdr a list of events
\(integers and symbols)."
  (let (bindings separated)
    (kmu-map-keymap
     (lambda (key def)
       (let ((desc (kmu-naked-key-description key)))
         (cond ((consp key)
                (push (list def desc) bindings))
               ((or (memq (aref key 0) (cdr exclude))
                    (and (car exclude)
                         (string-match (car exclude) desc))))
               ((or (memq (aref key 0) (cdr separate))
                    (and (car separate)
                         (string-match (car separate) desc)))
                (push (list def desc) separated))
               (t
                (let ((same (assq def bindings)))
                  (if same
                      (setcdr same (cons desc (cdr same)))
                    (push (list def desc) bindings)))))))
     keymap)
    (flet ((merge-range
            (lst mods &optional range)
            (setq range
                  (mapcan
                   (lambda (key)
                     (when (string-match (format "\\<%s[0-9]$" mods) key)
                       (list key)))
                   (cdr lst)))
            (cond
             ((= (length range) 10)
              (dotimes (i 10)
                (delete (format "%s%s" mods i) lst))
              (nconc lst (list (format "%s0..%s9" mods mods))))
             ((and (= (length range) 9)
                   (not (member (format "%s0" mods) lst)))
              (dotimes (i 9)
                (delete (format "%s%s" mods (1+ i)) lst))
              (nconc lst (list (format "%s1..%s9" mods mods)))))))
      (dolist (b bindings)
        (merge-range b "")
        (merge-range b "C-")
        (merge-range b "M-")
        (merge-range b "C-M-")
        (merge-range b "kp-")))
    (setq bindings (nconc bindings separated))
    (if order
        (mapcar
         (lambda (b)
           (cons (car b)
                 (sort (cdr b)
                       (apply-partially 'kmu-sort-keys-predicate order))))
         bindings)
      bindings)))

(defun kmu-sort-keys-predicate (order a b)
  "Return t if A appears earlier than B in ORDER, nil otherwise.
ORDER has to be a list of events or a list of such lists.
Instead of single events A and B can also be event sequences, in
which case the heads are "
  (let ((la (mapcar (lambda (k) (length (member k order))) (split-string a " ")))
        (lb (mapcar (lambda (k) (length (member k order))) (split-string b " ")))
        (ret nil))
    (while (or la lb)
      (cond ((not la)              (setq ret t   la nil lb nil))
            ((not lb)              (setq ret nil la nil lb nil))
            ((> (car la) (car lb)) (setq ret t   la nil lb nil))
            ((< (car la) (car lb)) (setq ret nil la nil lb nil))
            ((= (car la) (car lb)) (setq la (cdr la)
                                         lb (cdr lb)))
            (t (error "Impossible event sort error"))))
    ret))

(provide 'keymap-naked)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; keymap-naked.el ends here
