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
	
##################
## Global evals ##
##################

eval "$(starship init bash)"

#######################
## Machine-dependend ##
#######################

OPSYS=$(uname -s)
case $OPSYS in
    Darwin)
	alias vi='/usr/local/bin/emacs -nw'
        alias vim='/usr/local/bin/vim'
        alias cdr='cd /Users/Kristof/Documents/_Repositories'
        alias cdo='cd /Users/Kristof/ownCloud'
        alias e='/usr/local/bin/emacs -nw'
        alias python=/usr/local/bin/python3.8
        ## If powerline is used instead of, e.g., starship
        # powerline-daemon -q
        # POWERLINE_BASH_CONTINUATION=1
        # POWERLINE_BASH_SELECT=1
        # source /usr/local/lib/python3.8/site-packages/powerline/bindings/bash/powerline.sh
	export PATH="$PATH:$HOME/.gem/ruby/2.6.0/bin:/Applications/Visual Studio Code.app/Contents/Resources/app/bin"
	;;
    Linux)
	if [ $(uname -n) == "schopenhauer" ]; then
	     ## Some Ubuntu standard settings
	     # make less more friendly for non-text input files, see lesspipe(1)
	     [ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

	     # set variable identifying the chroot you work in (used in the prompt below)
	     if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
		 debian_chroot=$(cat /etc/debian_chroot)
	     fi

	     # set a fancy prompt (non-color, unless we know we "want" color)
	     case "$TERM" in
		 xterm-color|*-256color) color_prompt=yes;;
	     esac

	     if [ -n "$force_color_prompt" ]; then
		 if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
		     # We have color support; assume it's compliant with Ecma-48
		     # (ISO/IEC-6429). (Lack of such support is extremely rare, and such
		     # a case would tend to support setf rather than setaf.)
		     color_prompt=yes
		 else
		     color_prompt=
		 fi
	     fi

	     if [ "$color_prompt" = yes ]; then
		 PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
	     else
		 PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
	     fi
	     unset color_prompt force_color_prompt

	     # If this is an xterm set the title to user@host:dir
	     case "$TERM" in
		 xterm*|rxvt*)
		     PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
		     ;;
		 *)
		     ;;
	     esac
	     
	     # enable color support of ls and also add handy aliases
	     if [ -x /usr/bin/dircolors ]; then
		 test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
		 alias ls='ls --color=auto'
		 #alias dir='dir --color=auto'
		 #alias vdir='vdir --color=auto'
		 
		 alias grep='grep --color=auto'
		 alias fgrep='fgrep --color=auto'
		 alias egrep='egrep --color=auto'
	     fi
	     # Add an "alert" alias for long running commands.  Use like so:
	     #   sleep 10; alert
	     alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'
	     
	     # Alias definitions.
	     # You may want to put all your additions into a separate file like
	     # ~/.bash_aliases, instead of adding them here directly.
	     # See /usr/share/doc/bash-doc/examples in the bash-doc package.
	     
	     if [ -f ~/.bash_aliases ]; then
		 . ~/.bash_aliases
	     fi
	     
	     # enable programmable completion features (you don't need to enable
	     # this, if it's already enabled in /etc/bash.bashrc and /etc/profile
	     # sources /etc/bash.bashrc).
	     if ! shopt -oq posix; then
		 if [ -f /usr/share/bash-completion/bash_completion ]; then
		     . /usr/share/bash-completion/bash_completion
		 elif [ -f /etc/bash_completion ]; then
		     . /etc/bash_completion
		 fi
	     fi
	     
	     
	     alias cdr='cd /media/kristof/misc/repositories'
	     alias cdhci='cd /home/kristof/shares/hci'
	     alias cdmcm='cd /home/kristof/shares/mcm'
	     alias cdoc='cd /home/kristof/shares/oc'
	     alias open='xdg-open'
	     
	     export EDITOR="/home/kristof/.local/bin/vv"
	     
	     PATH=$HOME/.local/bin:~/anaconda3/bin:~/.cabal/bin:$PATH

             ## If powerline is used instead of, e.g., starship
	     # export POWERLINE_COMMAND=powerline
	     # export POWERLINE_CONFIG_COMMAND=powerline-config
	     # POWERLINE_BASH_CONTINUATION=1
	     # POWERLINE_BASH_SELECT=1
	     # . ~/.local/lib/python2.7/site-packages/powerline/bindings/bash/powerline.sh
	     # 
	     # if [ -f ~/.local/lib/python2.7/site-packages/powerline/bindings/bash/powerline.sh ]; then
	     #     source ~/.local/lib/python2.7/site-packages/powerline/bindings/bash/powerline.sh
	     # fi
	fi
esac
