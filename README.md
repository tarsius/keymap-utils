# keymap-utils.el --- keymap utilities

Copyright (C) 2008, 2009  Jonas Bernoulli

* Author: Jonas Bernoulli <jonas@bernoul.li>
* Created: 20080830
* Updated: 20090313
* Version: 0.1.1
* Homepage: http://github.com/tarsius/keymap-utils
* Keywords: convenience, extensions

This file is not part of GNU Emacs.

This file is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

This file is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

# Commentary

This library provides additional keymap predicates and other keymap
functions.

This library assumes that the char-table of a full keymap is to be
found in its cadr.  This actually does not have to be the case but
it is for keymaps that have been created using the standart mechanism
for creating and modifying keymaps.  Key lookup in a full keymap that
has other bindings before the char-table still works as expected, but
since it is a common assumption that the char-table is in the cadr
(the doc-string of `make-keymap' states that full keymaps have the
form (keymap CHARTABLE . ALIST) ), it is legimate that I make this
assumtion here also.  If your library has problems because of this
then it probably should be fixed; not `keymap-utils'.

This library is not intended to be used with menus, before using a
function with a menu make sure it still behaves as you intended.



