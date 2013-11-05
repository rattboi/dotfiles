#!/bin/bash 

PWD=$(pwd)

# get submodule shit set up
git submodule init
git submodule update 

# link zsh things
ln -s $PWD/oh-my-zsh ~/.oh-my-zsh
ln -s $PWD/zshrc ~/.zshrc

# link vim things
ln -s $PWD/vimrc ~/.vimrc
ln -s $PWD/vim ~/.vim
vim +BundleInstall +qall

# link awesome things
mkdir -p ~/.config
ln -s $PWD/awesome ~/.config/awesome

# link all the other stuff
ln -s $PWD/tmux.conf ~/.tmux.conf
ln -s $PWD/pentadactylrc ~/.pentadactylrc
ln -s $PWD/Xdefaults ~/.Xdefaults
