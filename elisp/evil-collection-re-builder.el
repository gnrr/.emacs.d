;;; evil-collection-re-builder.el --- Evil bindings for re-builder -*- lexical-binding: t -*-

;; Copyright (C) 2020 Hideaki Shishido

;; Author: Pierre Neidhardt <ambrevar@gmail.com>
;; Maintainer: James Nguyen <james@jojojames.com>, Pierre Neidhardt <ambrevar@gmail.com>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: evil, calc, tools

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Evil bindings for re-builder.

;;; Code:
(require 'evil-collection)
(require 're-builder)

(defconst evil-collection-re-builder-maps '(reb-mode-map))

;; based reb-copy
(defun evil-collection-re-builder-reb-copy-stripped ()
  "Copy current RE into the kill ring for later insertion. Double quotes are stripped."
  (interactive)
  (reb-update-regexp)
  (let ((re (with-output-to-string
	      (print (reb-target-binding reb-regexp)))))
    (kill-new (substring re 2 (- (length re) 2)))
    (message "Regexp copied to kill-ring (stripped)")))

(defun evil-collection-re-builder-setup ()
  "Set up `evil' bindings for `re-builder'."
  (evil-collection-inhibit-insert-state 'reb-mode-map)
  (evil-set-initial-state 'reb-mode 'insert)
    "q" 'reb-quit
    "y" 'evil-collection-re-builder-reb-copy-stripped
    "Y" 'reb-copy
    "n" 'reb-next-match
    "N" 'reb-prev-match
    "\C-c\C-c" 'reb-toggle-case
    "\C-c\C-i" 'reb-change-syntax
    "\C-c\C-e" 'reb-enter-subexp-mode
    "\C-c\C-b" 'reb-change-target-buffer
    "\C-c\C-u" 'reb-force-update)

(provide 'evil-collection-re-builder)
;;; evil-collection-re-builder.el ends here
