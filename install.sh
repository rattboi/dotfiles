#!/usr/bin/env bash 

PWD=$(pwd)
EASYFILES="oh-my-zsh zshrc vimrc vim tmux pentadactylrc Xdefaults"

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

# initialize vim
vim +BundleInstall +qall
