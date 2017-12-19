# "Sexy sh prompt" (modeled after "Sexy bash prompt")
#
# Tries to work on as many Bourne-style and Korn-style shells as
# possible and tries to be simple and not litter your environment.
#
# When you source this script in your (interactive) shell startup
# script (or on the command line), you can set some (transient)
# environment variables to configure a few extra options; see the
# examples below to get the full idea.
#
# Example usages:
#
# $ . ~/.sexy-sh-prompt.sh               # default is non-bold, no git prompt
# $ bold=1 git=1 . ~/.sexy-sh-prompt.sh  # bold prompt, git prompt
# $ bold= . ~/.sexy-sh-prompt.sh         # turns off bold prompt (say, you had it on before)
#
# This is what I have in my .shrc:
#
#   git=1 . ~/.sexy-sh-prompt.sh
#
# which gives me a "git prompt" (i.e., the checked-out branch is
# displayed in the prompt when you are in a git-tracked tree).  See
# the note below for extra files that are required for the "git
# prompt".

#-----------------------------------------------------------------------
# Git prompt

# Having a "git prompt" requires helper functions that need to be
# sourced out of git-prompt.sh (or .git-prompt.sh).  This can be found
# on your system under the git install directories or on the
# intertubes at
#
# https://git.kernel.org/pub/scm/git/git.git/tree/contrib/completion/git-prompt.sh
#
# However, it won't work with Korn shells, so qbit made a ksh version
# available as a github gist:
#
# https://gist.github.com/qbit/5483415
#
# I downloaded both of them and named the Korn-shell-variant
# .git-prompt.ksh and put them in my home directory, and I use the
# following logic to point to the correct one.

if [ "$git" ]; then
    if [ "${0%ksh}" != "$0" ]; then
        test -r ~/.git-prompt.ksh && . ~/.git-prompt.ksh
    else
        test -r ~/.git-prompt.sh && . ~/.git-prompt.sh
    fi
    GIT_PS1_SHOWDIRTYSTATE="${GIT_PS1_SHOWDIRTYSTATE:-1}"
    export GIT_PS1_SHOWDIRTYSTATE
else
    function __git_ps1 { :; }
fi

#-----------------------------------------------------------------------
# Build prompt string

# Here are the color strings.  `cNAME` means the `c`olor for `NAME`
# which is semantic.  Hence, cUSER is the color used for displaying
# the user ($USER) portion, and so on.  There's a 'default color"
# called `cDEFAULT` (surprise!).  Printing `$cNONE` will turn off ANSI
# colors (as presumably) text will display in the colors set by your
# terminal application (although I have not fully tested this).

cNONE=$(echo -e "\\033[0m")
cDEFAULT=$(c=246;[ "$bold" ]&&b=1||b=0;echo -e "\\033[0${b};38;5;${c}m")
cUSER=$(   c=208;[ "$bold" ]&&b=1||b=0;echo -e "\\033[0${b};38;5;${c}m")
cHOST=$(   c=214;[ "$bold" ]&&b=1||b=0;echo -e "\\033[0${b};38;5;${c}m")
cPWD=$(    c=190;[ "$bold" ]&&b=1||b=0;echo -e "\\033[0${b};38;5;${c}m")
cGIT=$(    c=183;[ "$bold" ]&&b=1||b=0;echo -e "\\033[0${b};38;5;${c}m")

# Here are the strings of each element of the prompt: the user
# portion, the host portion, and so on.  The definition for `PS1_PWD`
# demonstrates a nice hack for shells that cannot intrinsically
# shorten `$HOME` to `~`.  It's not mine and I have forgotten where I
# got it from.  (Sorry.)

PS1_USER="${cUSER}${USER}${cDEFAULT}"
PS1_HOST="${cHOST}$(uname -n | cut -d . -f 1)/$(uname -s)${cDEFAULT}"
PS1_PWD="${cPWD}"'${tilde[0${one#${PWD##"$HOME"*}1}]}${PWD#"$HOME"}'"${cDEFAULT}"
PS1_GIT='$(__git_ps1 " on '"${cGIT}"'%s'"${cDEFAULT}"'")'

one=1
tilde='~'
PS1='
'"${PS1_USER} at ${PS1_HOST} in ${PS1_PWD}${PS1_GIT}"'
[$?] $ '"${cNONE}"
export one tilde PS1

unset cNONE cDEFAULT cUSER cHOST cPWD cGIT
unset PS1_USER PS1_HOST PS1_PWD PS1_GIT
