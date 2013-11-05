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
Bundle 'vim-scripts/paredit.vim'
Bundle 'kien/rainbow_parentheses.vim'
Bundle 'tpope/vim-fireplace'
Bundle 'tpope/vim-classpath'
Bundle 'tpope/vim-fugitive'
Bundle 'airblade/vim-gitgutter'
Bundle 'scrooloose/syntastic'

Bundle 'guns/vim-clojure-static'
Bundle 'vim-scripts/verilog_systemverilog.vim'

filetype plugin indent on     " required!

syntax on
set hlsearch

" allow backspacing over everything in insert mode
set backspace=indent,eol,start

" numbers
set number
set numberwidth=4

set nobackup
set history=50      " keep 50 lines of command line history
set ruler           " show the cursor position all the time
set showcmd         " display incomplete commands
set incsearch       " do incremental searching

set tabstop=4
set softtabstop=4
set shiftwidth=4
set expandtab
set smartindent

set statusline=%F%m%r%h%w\ 
set statusline+=%=
set statusline+=%{fugitive#statusline()}
set statusline+=\ [line\ %l\/%L]
set laststatus=2

" case only matters with mixed case expressions
set ignorecase
set smartcase

" Rainbow Parentheses settings
au VimEnter * RainbowParenthesesToggle
au Syntax * RainbowParenthesesLoadRound
au Syntax * RainbowParenthesesLoadSquare
au Syntax * RainbowParenthesesLoadBraces
