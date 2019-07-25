#!/bin/bash
# Copyright (C) 2019 Miquel Sabaté Solà <mikisabate@gmail.com>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

# This is only here because the linters on GNU Emacs 25.x behave differently
# than in 26.x. I'm taking GNU Emacs 26.x as the source of truth for linters.

set -ex

if [ -z "$CI" ]; then
    exit 0
fi

cd "$( cd "$( dirname "$0" )/.." && pwd )"
if [ "$EVM_EMACS" = "emacs-25.1-travis" ] ||
       [ "$EVM_EMACS" = "emacs-25.2-travis" ] ||
       [ "$EVM_EMACS" = "emacs-25.3-travis" ]; then
    make git-validation
    make unit-test
else
    make test
fi
