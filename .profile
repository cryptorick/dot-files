PATH=$HOME/bin:$HOME/local/bin:$PATH

# set ENV to a file invoked each time sh is started for interactive use.
#ENV=$HOME/.shrc; export ENV

# Enable colors and such for git diffs
MORE="-erX" ; export MORE

# Set VIM as default
if [ -e "/usr/local/bin/vim" ]; then
  alias vi="vim"
fi

TERM=xterm-256color
XDG_CONFIG_HOME=~/.config
EDITOR=vi
PAGER=less
LESS="-RS"
export TERM XDG_CONFIG_HOME EDITOR PAGER LESS

export GOPATH=$(go env GOPATH 2>/dev/null)
[ "${GOPATH}" ] && PATH="${PATH}:${GOPATH}/bin"

# From: https://github.com/akermu/emacs-libvterm#shell-side-configuration
vterm_printf(){
    if [ -n "$TMUX" ]; then
        # Tell tmux to pass the escape sequences through
        # (Source: http://permalink.gmane.org/gmane.comp.terminal-emulators.tmux.user/1324)
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}

# For nix
export LD_LIBRARY_PATH=/usr/lib
##if [ ! -r .R/Makevars ]; then
##cat > .R/Makevars <<EOF
##LDFLAGS += -L${HOME}/.nix-profile/lib
##CPPFLAGS += -I${HOME}/.nix-profile/include
##ODBC_INCLUDE = ${HOME}/.nix-profile/include
##ODBC_LIBS = ${HOME}/.nix-profile/lib
##EOF
##fi
### You need this for RODBC configure step:
##export LDFLAGS=-L${HOME}/.nix-profile/lib
### Trying to following for the R curl package build.
##export PKG_CONFIG_PATH=/usr/lib/pkgconfig:/usr/share/pkgconfig

PS1="\$PWD $ "

. ~/sexy-sh-prompt 0 1

# Import colorscheme from 'wal' asynchronously
# &   # Run the process in the background.
# ( ) # Hide shell job control messages.
#(cat ~/.cache/wal/sequences &)
