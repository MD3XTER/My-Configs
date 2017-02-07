"set term=xterm-256color
set noswapfile
set t_Co=256

"execute pathogen#infect()
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

" Shortcut to rapidly toggle `set list`
nmap <leader>l :set list!<CR>
" "
" " " Use the same symbols as TextMate for tabstops and EOLs set
"listchars=tab:»\ ,trail:·,nbsp:·
set list
"
set wildignore+=*/tmp/*,*.so,*.swp,*.zip     " MacOSX/Linux
set wildignore+=*\\tmp\\*,*.swp,*.zip,*.exe  " Windows
"
let g:ctrlp_custom_ignore = '\v[\/]data|node_modules$'
"
let g:airline#extensions#tabline#enabled = 1
set laststatus=2
"
nnoremap <F5> :UndotreeToggle<cr>
"
noremap <Up> <NOP>
noremap <Down> <NOP>
noremap <Left> <NOP>
noremap <Right> <NOP>
"let g:vimrubocop_config = '/home/david/gov_congress/.ruby-style.yml'
"
" " rspec mappings
" map <Leader>t :call RunCurrentSpecFile()<CR>
" map <Leader>s :call RunNearestSpec()<CR>
" map <Leader>l :call RunLastSpec()<CR>
"
" let g:rspec_command = "!zeus rspec {spec}"
