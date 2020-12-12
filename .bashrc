## Global exports
export PATH="$PATH:$HOME/.local/bin"
export LC_CTYPE="de_DE.UTF-8"
export CLICOLOR=YES
export LSCOLORS=ExFxBxDxCxegedabagacad

## Global aliases

alias ..='cd ..'
alias ll="ls -lah"
alias config='/usr/bin/git --git-dir=$HOME/dotfiles/ --work-tree=$HOME'

## Machine-dependend
OPSYS=$(uname -n)
case $OPSYS in
    Space-K.local)
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
    
