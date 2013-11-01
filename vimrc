" Use Vim settings, rather than Vi settings (much better!).
" This must be first, because it changes other options as a side effect.
set nocompatible
filetype off                   " required!

set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

" let Vundle manage Vundle
" required! 
Bundle 'gmarik/vundle'

" My Bundles here:
"
Bundle 'tpope/vim-fireplace'
Bundle 'tpope/vim-classpath'
Bundle 'guns/vim-clojure-static'
Bundle 'vim-scripts/paredit.vim'

filetype plugin indent on     " required!

" allow backspacing over everything in insert mode
set backspace=indent,eol,start

set nobackup
set history=50		" keep 50 lines of command line history
set ruler		" show the cursor position all the time
set showcmd		" display incomplete commands
set incsearch		" do incremental searching

" In many terminal emulators the mouse works just fine, thus enable it.
if has('mouse')
  set mouse=a
endif

" Switch syntax highlighting on, when the terminal has colors
" Also switch on highlighting the last used search pattern.
if &t_Co > 2 || has("gui_running") && !exists("syntax_on")
  syntax on
  set hlsearch
endif

set nowrap

set tabstop=2
set shiftwidth=2
set expandtab
set smartindent

set laststatus=2

" case only matters with mixed case expressions
set ignorecase
set smartcase

" numbers
set number
set numberwidth=4
