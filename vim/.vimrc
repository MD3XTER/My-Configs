set term=xterm-256color
set noswapfile
set t_Co=256

syntax on
filetype plugin indent on
set hlsearch

colorscheme Tomorrow-Night-Bright

set nu
highlight LineNr ctermfg=grey
hi Visual  guifg=#000000 guibg=#FFFFFF gui=none

set shiftwidth=2
set expandtab
set smarttab
set tabstop=2

hi Normal ctermbg=none

set wildignore+=*/tmp/*,*.so,*.swp,*.zip     " MacOSX/Linux

let g:airline#extension#tabline#enabled = 1
set laststatus=2
let g:ctrlp_custom_ignore = '\v[\/]data|node_modules$'