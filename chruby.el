;;; chruby.el --- Emacs integration for chruby

;; Copyright (C) 2013 Arne Brasseur

;; Author: Arne Brasseur <arne@arnebrasseur.net>
;; URL: http://www.emacswiki.org/emacs/ChrubyEl
;; Version: 1.0
;; Created: 5 March 2013
;; Keywords: ruby chruby
;; EmacsWiki: ChrubyEl

;; This file is NOT part of GNU Emacs.

 ;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

 ;;; Attribution :

;; The chruby-change-path function was taken from rvm.el by Yves Senn

 ;;; Commentary:

;; The chruby function functions like the shell command, pass it the
;; name of a Ruby such as 'ruby-1.9.3' or 'jruby-1.7.2'.
;;
;; e.g.
;;
;; (require 'chruby)
;; (chruby "ruby-1.9.3")


(defvar chruby-current-ruby-binary-path nil
  "reflects the path to the current 'ruby' executable.
 This path gets added to the PATH variable and the exec-path list.")

(defvar chruby-current-ruby-name nil
  "The full name of the active Ruby, i.e. the name of the .rubies/
 subdirectory.")

(defun chruby-collect-rubies (rubies-dir)
  "Return the full directories found in the given directory"
  (cddr                    ; cddr : drop the first 2, in this case "." and ".."
   (and rubies-dir         ; not nil
        (and
         (file-directory-p rubies-dir)
         (directory-files rubies-dir t)))))

(defun chruby-rubies ()
  "Find all Rubies in various places, returns a list of directories."
  (mapcan
   'chruby-collect-rubies
   (list "/opt/rubies/"
         (concat (file-name-as-directory (getenv "HOME")) ".rubies")
         (getenv "RUBIES_DIR")
         "~/opt/rubies")))

(defun chruby-find (name)
  "Find the given ruby in the list of rubies"
  (first
   (delq nil
         (mapcar
          (lambda (ruby)
            (and (string-match name ruby) ruby))
          (chruby-rubies)))))

(defun chruby-change-path (new-binaries)
  (let ((current-binaries-for-path
         (mapconcat 'identity chruby-current-ruby-binary-path ":"))
        (new-binaries-for-path (mapconcat 'identity new-binaries ":")))
    (if (and chruby-current-ruby-binary-path
             (not (string= (first chruby-current-ruby-binary-path) "/bin")))
        (progn
          (setenv "PATH" (replace-regexp-in-string
                          (regexp-quote current-binaries-for-path)
                          new-binaries-for-path
                          (getenv "PATH")))
          (dolist (binary chruby-current-ruby-binary-path)
            (setq exec-path (remove binary exec-path))))
      (setenv "PATH" (concat new-binaries-for-path ":" (getenv "PATH"))))
    (dolist (binary new-binaries)
      (add-to-list 'exec-path binary))
    (setq eshell-path-env (getenv "PATH"))
    (setq chruby-current-ruby-binary-path new-binaries)))

(defun chruby-set-gemhome (gemhome gempath)
  (if (and gemhome gempath)
      (progn
        (setenv "GEM_HOME" gemhome)
        (setenv "GEM_PATH" gempath)
        (setenv "BUNDLE_PATH" gemhome))
    (setenv "GEM_HOME" "")
    (setenv "GEM_PATH" "")
    (setenv "BUNDLE_PATH" "")))

(defun chruby (&optional name)
  "If name is given, activate the given Ruby. Else, return the currently
 activated Ruby"
  (if name
      (chruby-activate name)
    (chruby-current)))

(defun chruby-activate (name)
  "Activate the given Ruby"
  (let ((ruby-root (chruby-find name)))
    (setq chruby-current-ruby-name (chruby-util-basename ruby-root))

    (chruby-change-path (list (concat ruby-root "/bin")))

    (let ((engine_version_gempath (chruby-query-version)))
      (let ((engine (first engine_version_gempath))
            (version (second engine_version_gempath))
            (gemroot (third engine_version_gempath)))
        (let ((gemhome (concat (getenv "HOME") "/.gem/" engine "/" version)))
          (chruby-set-gemhome gemhome
                              (concat gemhome ":" gemroot))
          (chruby-change-path
           (list (concat gemhome "/bin") (concat ruby-root "/bin"))))))))

(defun chruby-query-version ()
  "Shell out to Ruby to find out the current engine (ruby, jruby, etc), the
ruby version, and the gem path"
  (split-string
   (shell-command-to-string
    "ruby -rubygems -e 'print [(defined?(RUBY_ENGINE) ? RUBY_ENGINE : %[ruby]), (RUBY_VERSION), (Gem.default_dir.inspect)].join(%[##])'") "##"))

(defun chruby-util-basename (path)
  (file-name-nondirectory (directory-file-name path)))

(defun chruby-current ()
  "Return the currently activated Ruby name"
  chruby-current-ruby-name)

(provide 'chruby)
