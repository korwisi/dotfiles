####################
## Global exports ##
####################

export PATH="$PATH:$HOME/.local/bin"
export LC_CTYPE="de_DE.UTF-8"
export CLICOLOR=YES
export LSCOLORS=ExFxBxDxCxegedabagacad

# Don't put duplicate lines or lines starting with space in the history.
HISTCONTROL=ignoreboth

# Append to the history file, don't overwrite it
shopt -s histappend

# Setting history lenght and filesize
HISTSIZE=1000
HISTFILESIZE=2000

# Check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

####################
## Global aliases ##
####################

alias ..='cd ..'
alias ll="ls -lah"
alias config='/usr/bin/git --git-dir=$HOME/dotfiles/ --work-tree=$HOME'

## Global eval

## Machine-dependend
OPSYS=$(uname -s)
case $OPSYS in
    Darwin)
	alias vi='/usr/local/bin/emacs-27.1'
        alias vim='/usr/local/bin/vim'
        alias cdr='cd /Users/Kristof/Documents/_Repositories'
        alias cdo='cd /Users/Kristof/ownCloud'
        alias e='/usr/local/bin/emacs-27.1'
        alias python=/usr/local/bin/python3.8
        ## If powerline is used instead of, e.g., starship
        # powerline-daemon -q
        # POWERLINE_BASH_CONTINUATION=1
        # POWERLINE_BASH_SELECT=1
        # source /usr/local/lib/python3.8/site-packages/powerline/bindings/bash/powerline.sh
	eval "$(starship init bash)"
	;;
esac
