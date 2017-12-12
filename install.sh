#!/usr/bin/env bash 

PWD=$(pwd)
EASYFILES="oh-my-zsh zshrc vimrc Xdefaults tmux.conf urxvt ideavimrc stalonetrayrc xmonad bin khdrc chunkwmrc"

# get submodules set up
git submodule init
git submodule update 

# link easy things
for dotfile in $EASYFILES
do
    ln -s $PWD/$dotfile ~/.$dotfile
done

# link harder things
mkdir -p ~/.config
ln -s $PWD/awesome ~/.config/awesome

mkdir -p ~/.config/karabiner
ln -s $PWD/karabiner.json ~/.config/karabiner/karabiner.json

if [ -e "~/Library/Preferences/" ]; then
  mkdir -p ~/Library/Preferences/kitty
  ln -s $PWD/kitty.conf ~/Library/Preferences/kitty/kitty.conf
fi

