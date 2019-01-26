PATH=$HOME/bin:$HOME/local/bin:/sbin:/bin:/usr/sbin:/usr/bin:/usr/games:/usr/local/sbin:/usr/local/bin; export PATH

# set ENV to a file invoked each time sh is started for interactive use.
ENV=$HOME/.shrc; export ENV

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
export TERM XDG_CONFIG_HOME EDITOR PAGER

export GOPATH=$(go env GOPATH 2>/dev/null)
[ "${GOPATH}" ] && PATH="${PATH}:${GOPATH}/bin"

PS1="\$PWD $ "

. ~/sexy-sh-prompt 0 1

# Import colorscheme from 'wal' asynchronously
# &   # Run the process in the background.
# ( ) # Hide shell job control messages.
#(cat ~/.cache/wal/sequences &)
