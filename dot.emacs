;;; -*- coding:utf-8; mode:emacs-lisp -*-
;;;
;;; ~/.emacs.d/dot.emacs
;;;

;;; ** Windows **
;;; $ cp ~/.emacs.d/dot.emacs ~/.emacs
;;;   and
;;; Make new USER ENVIROMENT VARIABLE "HOME" --> "C:\Users\XXXX"
;;;
;;; ** Mac/Linux **
;;; $ ln -s ~/.emacs.d/dot.emacs ~/.emacs
;;;

(message "--> loading \"dot.emacs\"...")
(setq msg-succeeded-dot-emacs "<-- done    \"dot.emacs\"")

;;
;; package setting
;;
(require 'package)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(setq package-user-dir "~/.emacs.d/packages")
(package-initialize)
(unless (require 'use-package nil t)
  (defmacro use-package (&rest args)))

;;
;; load ~/.emacs.d/init.d
;;
(load "~/.emacs.d/init.el" nil t)

;;
;; customize setting
;;
(setq custom-file "~/.emacs.d/custom.el") ; write custom settings into external file instead of init.el
(load custom-file nil t)

(message msg-succeeded-dot-emacs)

;;
;; dot.emacs ends here
;;
