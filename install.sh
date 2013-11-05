#!/bin/sh 

# get submodule shit set up
git submodule init
git submodule update 

# link zsh things
ln -s `pwd`/oh-my-zsh ~/.oh-my-zsh
ln -s `pwd`/zshrc ~/.zshrc

# link vim things
ln -s `pwd`/vimrc ~/.vimrc
ln -s `pwd`/vim ~/.vim
vim +BundleInstall +qall

# link awesome things
mkdir ~/.config
ln -s `pwd`/awesome ~/.config/awesome

# link all the other stuff
ln -s `pwd`/tmux.conf ~/.tmux.conf
ln -s `pwd`/pentadactylrc ~/.pentadactylrc
ln -s `pwd`/Xdefaults ~/.Xdefaults
