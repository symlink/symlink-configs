# Set shell prompt
function prompt {
    local        BLACK="\[\033[0;30m\]"
    local         BLUE="\[\033[0;34m\]"
    local        GREEN="\[\033[0;32m\]"
    local         CYAN="\[\033[0;36m\]"
    local          RED="\[\033[0;31m\]"
    local       PURPLE="\[\033[0;35m\]"
    local        BROWN="\[\033[0;33m\]"
    local   LIGHT_GRAY="\[\033[0;37m\]"
    local    DARK_GRAY="\[\033[0;30m\]"
    local   LIGHT_BLUE="\[\033[0;36m\]"
    local  LIGHT_GREEN="\[\033[0;32m\]"
    local   LIGHT_CYAN="\[\033[0;36m\]"
    local    LIGHT_RED="\[\033[0;31m\]"
    local LIGHT_PURPLE="\[\033[0;35m\]"
    local       YELLOW="\[\033[0;33m\]"
    local        WHITE="\[\033[0;37m\]"
    local    NO_COLOUR="\[\033[0m\]"

    case $TERM in
	xterm*)
	    TITLEBAR='\[\033]0;\u@\H:\w\007\]'
	    ;;
	*)
	    TITLEBAR="symlink"
	    ;;
    esac

    case $1 in
	gaza)
	    PS1="${TITLEBAR}$GREEN[$LIGHT_PURPLE\u$LIGHT_GRAY$GREEN:$LIGHT_PURPLE\W$GREEN]$LIGHT_PURPLE\$>$NO_COLOUR "
	    ;;
        symlink)
	    PS1="${TITLEBAR}$LIGHT_RED[$LIGHT_PURPLE\u$LIGHT_RED@$LIGHT_RED\H: $LIGHT_BLUE\W $LIGHT_RED] $LIGHT_PURPLE\$>$NO_COLOUR "
			;;
	*)
	    PS1="[\u@:\w]\$ "
	    ;;
    esac

    PS2='> '

    PS4='+ '

}

case $HOSTNAME in
    *) 
	prompt symlink
	;;
esac

EDITOR="vim"
VISUAL="vim"

export PS1

export LS_COLORS='no=00:fi=00:di=00;33:ln=target:pi=40;33:so=01;35:bd=40;33;01:cd=40;33;01:or=40;31;01:ex=01;30:*.tar=01;31:*.tgz=01;31:*.arj=01;31:*.taz=01;31:*.lzh=01;31:*.zip=01;31:*.z=01;31:*.Z=01;31:*.gz=01;31:*.bz2=01;31:*.deb=01;31:*.rpm=01;31:*.jpg=01;35:*.png=01;35:*.gif=01;35:*.bmp=01;35:*.ppm=01;35:*.tga=01;35:*.xbm=01;35:*.xpm=01;35:*.tif=01;35:*.png=01;35:*.mpg=01;35:*.avi=01;35:*.fli=01;35:*.gl=01;35:*.dl=01;35:'
export LSCOLORS="exfxcxdxbxegedabagacad"

case `uname` in
	"Darwin")
		export TERM="xterm-color"
		export SANDBOX="${HOME}/Sites"
		CLASSPATH=${HOME}/Library/Java/Extensions
		JAVA_HOME=/Library/Java/Home
		PYTHONPATH=${HOME}/Library/Python:${HOME}/lib/python2.3/site-packages:${PYTHONPATH}
		PATH=/Volumes/iDisk/Documents/Scripts:${HOME}/bin/sh:/mamp/bin:/signal/apps/mysql/bin:$PATH
		;;

	"Linux")
		alias ls='ls -N --color=auto'
		export SANDBOX="/var/httpd/sandbox/bobby"
		;;

	"emacs")
		;;
esac

if [ $TERM == "emacs" ]; then
    export PS1="[\u:\H]\w > "
fi

if [ -f ~/.work ]; then
    source ~/.work
fi

if [ -f ~/.personal ]; then
    source ~/.personal
fi

if [ -f ~/.aliases ]; then
    source ~/.aliases
fi

export CLASSPATH JAVA_HOME CVSROOT CVS_RSH PYTHONPATH EDITOR VISUAL
export PATH=${HOME}/bin:/usr/local/bin:/usr/local/mysql/bin:$PATH:/usr/local/git/bin:/command

