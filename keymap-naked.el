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

(provide 'keymap-naked)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; keymap-naked.el ends here