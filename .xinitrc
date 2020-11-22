#!/bin/sh
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

xsetroot -solid black
xrdb -load .Xresources

#/path/to/pywal-venv/bin/wal -i /path/to/wallpaper.jpg
#polybar example &
#compton &
#start-pulseaudio-x11 &
#wal -R

exec /home/rick/builds/echinus/echinus
exec /home/rick/builds/dwm/dwm
exec /home/rick/bin/monsterwm-start
exec /usr/local/bin/spectrwm
exec /usr/X11R6/bin/cwm

# exec gnome-session
# exec startkde
# exec startxfce4
# ...or the Window Manager of your choice
