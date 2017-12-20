;;; cc-compiler.el --- A cc-compiler declaration for working with GCC

;; Copyright (C) 2016,2017 Vesselin Mladenov
;;
;; Author: Vesselin Mladenov <veselinm@gmail.com>
;; Maintainer: Vesselin Mladenov <veselinm@gmail.com>
;; Created: 11 Nov 2016
;; Modified: 20 Dec 2017
;; Version: 0.1.1
;; Package-Requires: ((s "1.11.0"))
;; Keywords: c, processes, tools
;; URL: https://github.com/vmladenov/cc-compiler

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; The `cc-compiler' provides functions to query C/C++ compiler variables
;;
;; Please see README.md from the same repository for documentation.

;;; Code:

(defgroup cc-compiler nil
  "A cc-compiler customization group."
  :group 'Programming)

(defcustom cc-compiler-debug-enable nil
  "Whether to display cc-compiler work in a *Messages* buffer."
  :type 'boolean
  :group 'cc-compiler)

(defcustom cc-compiler-type "gnu"
  "Type of the compiler."
  :type 'string
  :group 'cc-compiler)

(defcustom cc-compiler-path nil
  "Path to the compiler bundle.
If left empty the package will try to guess it."
  :type 'string
  :group 'cc-compiler)

(defcustom cc-compiler-triplet ""
  "Target host triplet (when cross-compiling).
Example: arm-unknown-eabihf-"
  :type 'string
  :group 'cc-compiler)

(defvar cc-compiler-cc nil)
(defvar cc-compiler-cpp nil)
(defvar cc-compiler-c++ nil)
(defvar cc-compiler-version nil)
(defvar cc-compiler-machine nil)
(defvar cc-compiler-version-major nil)
(defvar cc-compiler-system-include-path nil)

(defun chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (replace-regexp-in-string (rx (or (: bos (* (any " \t\n")))
                                    (: (* (any " \t\n")) eos)))
                            "" str))

(defun cc-compiler-debug (&rest args)
  "Print a message to *Messages* buffer.
FORMAT and ARGS are printf-like."
  (when cc-compiler-debug-enable
    (apply #'message args)))

(defun cc-compiler-invalidate ()
  "Invalidate previously set variables."
  (setq cc-compiler-cc nil
        cc-compiler-cpp nil
        cc-compiler-c++ nil))

(defun cc-compiler-guess-names ()
  "Guess the names of the C compiler executables."
  (let (gcc cpp g++)
    (cond ((string= cc-compiler-type "gnu")
           (setq gcc "gcc" cpp "cpp" g++ "g++")))
    (setq gcc (concat cc-compiler-triplet gcc)
          cpp (concat cc-compiler-triplet cpp)
          g++ (concat cc-compiler-triplet g++))
    (cl-values gcc cpp g++)))

(defun cc-compiler-guess-path ()
  "Try to guess the path to the compiler."
  (cl-multiple-value-bind (gcc cpp g++) (cc-compiler-guess-names)
    (when (not cc-compiler-path)
      (setq cc-compiler-path
            (file-name-directory (executable-find gcc))))
    (setq cc-compiler-cc (concat cc-compiler-path gcc)
          cc-compiler-cpp (concat cc-compiler-path cpp)
          cc-compiler-c++ (concat cc-compiler-path g++))
    (when (not (file-executable-p cc-compiler-cc))
      (cc-compiler-invalidate))
    cc-compiler-path))

(defun cc-compiler-exec (command)
  "Execute shell COMMAND and log result."
  (cc-compiler-debug "Executing: %s" command)
  (let ((res (shell-command-to-string command)))
    (cc-compiler-debug "Executing: %s: %s" command res)
    res))

(defun cc-compiler-system-include-path-get ()
  "Get the system include path list."
  (let ((command
         (concat "echo | " cc-compiler-cpp " -v 2>&1"
                 "| egrep '^\s[/0-9a-zA-Z_-\.]+$$' | sed s/^/-I/ | cut -c 4-")))
    (setq cc-compiler-system-include-path
          (split-string
           (cc-compiler-exec command) split-string-default-separators t))
    (cc-compiler-debug "Got system-include-path: %s"
                       cc-compiler-system-include-path)
    cc-compiler-system-include-path))

(defun cc-compiler-version-get ()
  "Get the version of the compiler."
  (let ((command
         (concat cc-compiler-cpp " -dumpversion")))
    (setq cc-compiler-version
          (chomp (cc-compiler-exec command)))
    (setq cc-compiler-version-major
          (car (split-string cc-compiler-version "\\." t)))
    (cc-compiler-debug "Got version: %s" cc-compiler-version)
    cc-compiler-version))

(defun cc-compiler-machine-get ()
  "Get the machine of the compiler."
  (let ((command
         (concat cc-compiler-cpp " -dumpmachine")))
    (cc-compiler-debug "Executing: %s" command)
    (setq cc-compiler-machine
          (chomp (shell-command-to-string command)))
    (cc-compiler-debug "Got machine: %s" cc-compiler-machine)
    cc-compiler-machine))

(defun cc-compiler-hook()
  "Setup the variables for the current buffer."
  (cc-compiler-guess-path)
  (cc-compiler-version-get)
  (cc-compiler-machine-get)
  (cc-compiler-debug "Opened C file [%s]" (buffer-file-name)))

(provide 'cc-compiler)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; cc-compiler.el ends here
