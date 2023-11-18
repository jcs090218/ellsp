#!/bin/bash

# Copyright (C) 2023 the Ellsp authors.

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3, or (at your option)
# any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

if [ "$OS" = "Windows_NT" ]; then
  target="ellsp-win.exe"
else
  case $(uname -sm) in
    "Darwin x86_64") target="ellsp-macos" ;;
    "Darwin arm64") target="ellsp-macos" ;;
    "Linux aarch64") target="ellsp-linux" ;;
    *) target="ellsp-linux" ;;
  esac
fi

SDIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

curl -fsSL https://github.com/jcs-elpa/ellsp/releases/download/0.0.0/${target} -o ${SDIR}/${target}
