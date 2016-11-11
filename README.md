# cc-compiler
 The 'cc-compiler' provides functions to query C/C++ compiler variables from GNU Emacs

## Installation

The package available on:
* [github](https://github.com/vmladenov/cc-compiler.git)

You can install it by executing

`M-x package-install cc-compiler`

or download it from the github URL.

You will need the [s.el](https://github.com/magnars/s.el) package installed.

## Activation

``` cl
(require 'cc-compiler)
(cc-compiler-query)
```

## Variables

The following variables are exported:

- `cc-compiler-compiler-version ;; =>  Compiler's version aka -dumpversion for GCC `
- `cc-compiler-compiler-machine  ;; => Compiler's machine aka -dumpmachine for GCC`
- `cc-compiler-system-include-path ;; => List of compiler's system include paths`
- `cc-compiler-compiler-version-major ;; =>  Compiler's major version number`

## Functions

- `cc-compiler-query () ;; => Set the exported variables `

## License

Copyright (C) 2016 Vesselin Mladenov

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License as
published by the Free Software Foundation; either version 3, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
