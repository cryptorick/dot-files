#!/bin/sh

XDG_APP_DIRS="${XDG_APP_DIRS-/usr/local/share/applications .local/share/applications}"

_display_apps () {
    ls -1 ${XDG_APP_DIRS} |
        grep -E 'desktop$' |
        sed 's/\.desktop$//' |
        sort |
        column >&2
}

_display_specific_apps () {
    for d in ${XDG_APP_DIRS}; do
        grep -l "$1" "$d"/*.desktop
    done |
        sed 's?.*/??' |
        sort |
        column >&2
}

if [ -z "$1" ]; then
    cat >&2 <<EOF
Usage: ${0##*/} filename

  Displays filetype and app associated to filename; then shows user
  how to invoke a command to change the app associated to this
  file(type).

Available apps on this host are:
EOF
    _display_apps
    exit 1
elif [ ! -r "$1" ]; then
    echo "File $1 is not readable.  Bailing." >&2
    exit 2
fi

filetype=$(xdg-mime query filetype "$1")

echo "Filetype is : ${filetype}" >&2
echo "App is      :" $(xdg-mime query default "$filetype") >&2
echo >&2
echo "Now try this command to reassign this filetype to \${newapp}:" >&2
echo "$ xdg-mime default \${newapp}.desktop ${filetype}" >&2
echo >&2
echo "Possible values for \${newapp} for type ${filetype} are:" >&2
_display_specific_apps "${filetype}"
echo >&2
echo "Available apps on this host are:" >&2
_display_apps
