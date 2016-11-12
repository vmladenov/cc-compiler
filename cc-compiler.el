;;; cc-compiler.el --- A cc-compiler declaration for working with GCC

;; Copyright (C) 2016 Vesselin Mladenov
;;
;; Author: Vesselin Mladenov <veselinm@gmail.com>
;; Maintainer: Vesselin Mladenov <veselinm@gmail.com>
;; Created: 11 Nov 2016
;; Modified: 11 Nov 2016
;; Version: 0.1.0
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

(require 's)

(defgroup cc-compiler nil
  "A cc-compiler customization group."
  :group 'Programming)

(defcustom cc-compiler-debug nil
  "Whether to display cc-compiler work in a *Messages* buffer."
  :type 'boolean
  :group 'cc-compiler)

(defvar cc-compiler-compiler-stddef nil)
(defvar cc-compiler-compiler-version nil)
(defvar cc-compiler-compiler-machine nil)
(defvar cc-compiler-system-include-path nil)
(defvar cc-compiler-compiler-version-major nil)

(defun cc-compiler-query ()
  "Get the system include path from the compiler"
  (let ((compiler
         (s-chomp
          (shell-command-to-string "which gcc")))
        (preprocessor
         (s-chomp
          (shell-command-to-string "which cpp"))))
    (when cc-compiler-debug
      (message "Compiler : %s" compiler)
      (message "Preprocessor : %s" preprocessor))
    (let ((command
           (s-concat
            (s-concat "echo | " preprocessor " -v 2>&1")
            (s-concat "| egrep '^\s[/0-9a-zA-Z_-\.]+$$' | sed s/^/-I/ | cut -c 4-"))))
      (when cc-compiler-debug
        (message "Executing : %s" command))
      (setq cc-compiler-system-include-path
            (s-lines
             (shell-command-to-string command)))
      (when cc-compiler-debug
        (message "system-include-path : %s" cc-compiler-system-include-path)))
    (let ((command
           (s-concat compiler " -dumpversion")))
      (when cc-compiler-debug
        (message "Executing : %s" command))
      (setq cc-compiler-compiler-version
            (s-chomp
             (shell-command-to-string command)))
      (when cc-compiler-debug
        (message "Compiler version : %s" cc-compiler-compiler-version)))
    (let ((command
           (s-concat compiler " -dumpmachine")))
      (when cc-compiler-debug
        (message "Executing : %s" command))
      (setq cc-compiler-compiler-machine
            (s-chomp
             (shell-command-to-string command)))
      (when cc-compiler-debug
        (message "Compiler machine : %s" cc-compiler-compiler-machine)))

    (let ((command
           (s-concat compiler " -E - <<<'#include<stddef.h>' | grep stddef")))
      (when cc-compiler-debug
        (message "Executing : %s" command))
      (setq cc-compiler-compiler-stddef
            (s-chomp
             (car
              (nthcdr
               2
               (s-split
                " "
                (s-trim
                 (car
                  (s-lines
                   (shell-command-to-string "gcc -E - <<<'#include<stddef.h>' | grep stddef")))))))))
      (when cc-compiler-debug
        (message "stddef.h : %s" command)))
    (setq cc-compiler-compiler-version-major
          (s-match "^[0-9]+" cc-compiler-compiler-version))
    (when cc-compiler-debug
      (message "Compiler version major : %s" cc-compiler-compiler-version))))

(provide 'cc-compiler)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; cc-compiler.el ends here
