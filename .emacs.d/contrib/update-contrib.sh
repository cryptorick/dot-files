#!/bin/sh
#
# Purpose: install/update contrib software not found in elpa.
#
# This script will install the contrib software into the same
# directory in which it itself is located.
#
# Assumptions:
#
# - This script is located in a direct subdirectory of ~/.emacs.d.
#
# - source repos are indicated in the *.el config files, prefixed by
#   the string '; Source: '.

_MYLOC=${0%/*}

cd "${_MYLOC}"

_CONFIGFILES="../*.el ../lisp/*.el"

grep -hE '; Source: ' ${_CONFIGFILES} |
    awk '{print $NF}' |
    while read _URL; do
        _DIR=${_URL##*/}; _DIR=${_DIR%.git}
        if [ "$1" = "list" ]; then
            printf "%-30s %s\n" "${_DIR}" "${_URL}"
            continue
        fi
        echo "Checking ${_DIR} ..." >&2
        if [ -d "${_DIR}" ]; then
            echo "  ... checking for updates ..." >&2
            (cd ${_DIR} && git pull 2>&1) | sed 's/^/  /' >&2
        else
            echo "  ... cloning from source (${_URL}) ..." >&2
            git clone ${_URL} 2>&1 | sed 's/^/  /' >&2
        fi
        echo "... DONE" >&2
    done
