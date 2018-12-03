" Plug {{{
call plug#begin('~/.local/share/nvim/plugged')

Plug 'equalsraf/neovim-gui-shim'
Plug 'frankier/neovim-colors-solarized-truecolor-only'

Plug 'mileszs/ack.vim'
Plug 'neomake/neomake'
Plug 'junegunn/fzf'
Plug 'junegunn/fzf.vim'

Plug 'xolox/vim-misc'
Plug 'xolox/vim-session'
" Plug 'xolox/vim-easytags'

Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'Shougo/neosnippet'
Plug 'Shougo/neosnippet-snippets'

Plug 'tomtom/tcomment_vim'
Plug 'junegunn/goyo.vim' " distraction free

Plug 'tpope/vim-fugitive'
Plug 'scrooloose/nerdtree'
Plug 'tmhedberg/SimpylFold'

Plug 'easymotion/vim-easymotion'  " like vimium
Plug 'godlygeek/tabular'
Plug 'gorkunov/smartpairs.vim'    " viv
Plug 'henrik/vim-indexed-search'  " match M of N
Plug 'thinca/vim-ref'
Plug 'tmhedberg/matchit'          " use % to match more things
Plug 'brooth/far.vim'             " find and replace

Plug 'Yggdroot/indentLine'

Plug '$HOME/project/vim-senter', { 'do': ':UpdateRemotePlugins' }
Plug '$HOME/project/doc-browser/nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'raghur/vim-ghost', {'do': ':GhostInstall'}

Plug 'waiting-for-dev/vim-www'

" Plug 'terryma/vim-multiple-cursors'

" haskell
" Plug 'eagletmt/neco-ghc'
Plug 'neovimhaskell/haskell-vim'
Plug 'autozimu/LanguageClient-neovim', {
    \ 'branch': 'next',
    \ 'do': './install.sh' }

" elm
Plug 'elmcast/elm-vim'

" python
Plug 'hynek/vim-python-pep8-indent'
Plug 'zchee/deoplete-jedi'
Plug 'tell-k/vim-autopep8'
Plug 'davidhalter/jedi-vim'
Plug 'majutsushi/tagbar'


Plug 'peterhoeg/vim-qml'

call plug#end()
" }}}

" Vim Options {{{
scriptencoding utf-8
filetype plugin on
filetype indent on
syntax on

set background=dark
colorscheme solarized
set guicursor=
set title

let mapleader=","
let maplocalleader="\\"

" File related
set fileformat=unix
set fileformats=unix,dos
set fileencoding=utf-8
set fileencodings=utf-8,gbk
set encoding=utf-8
set autoread
autocmd BufEnter,FocusGained * checktime
" set swapfile
" set backupdir=/tmp//,.
set autowrite
" set confirm

set number
set incsearch
set hlsearch
set ic
set history=10000
set laststatus=2
set showcmd
set wildignorecase

set conceallevel=0
set concealcursor=nc
hi clear Conceal

" Space and indent
set expandtab
set tabstop=2
set softtabstop=2
set shiftwidth=2
set smarttab
set backspace=indent,eol,start
set smartindent
set autoindent

set hidden

" Long lines
set breakindent
" Indent the wrapped line

set linebreak
set nowrap
" When 'wrap' is off, use '»' as the last column to indicate a wrap
set listchars=extends:»

highlight! link NonText Character
" set showbreak=↳

set termguicolors

set splitright
set mouse=a
" }}}

" Key Bindings {{{
" C-S
noremap  <C-S> :update<CR>
vnoremap <C-S> <C-C>:update<CR>
inoremap <C-S> <C-O>:update<CR>

" Abbr
iabbrev todo: TODO @incomplete:
iabbrev fixme: FIXME @incomplete:

inoremap .. ->
inoremap ... ..
inoremap ,, <-
" inoremap >> =>
" inoremap >>> >>=

inoremap ;; ::
inoremap \\ (\)
tnoremap <ESC> <C-\><C-n>


" For markdown headers
inoremap <A-1> <ESC>VypVr=k$jo<ESC>o
inoremap <A-2> <ESC>VypVr-k$jo
nnoremap <A-1> <ESC>VypVr=k$jo<ESC>
nnoremap <A-2> <ESC>VypVr-k$jo<ESC>

" Pair
inoremap '' ''<ESC>i
inoremap "" ""<ESC>i
inoremap `` ``<ESC>i
inoremap <> <><ESC>i
inoremap ( ()<ESC>i
inoremap [ []<ESC>i
inoremap { {}<ESC>i
" inoremap gul «
" inoremap gur »
" inoremap gup «»<ESC>i

nnoremap ; :
vnoremap ; :
nnoremap : ;
vnoremap : ;

nnoremap <A-,> ^
nnoremap <A-.> $
nnoremap Y y$

" temporary, see https://github.com/equalsraf/neovim-qt/issues/215
vnoremap <C-C> "+y
inoremap <C-V> <ESC>"+pa
xnoremap <silent> p p:let @"=@0<CR>
" vnoremap <C-Insert> "+y
" nnoremap <S-Insert> "+p
" inoremap <S-Insert> <ESC>"+pa

" Toggle command line window
noremap <A-;> q:
autocmd CmdwinEnter * noremap <buffer> <ESC> <ESC>:q<CR>

" Open help in new tab
cnoreabbrev <expr> h getcmdtype()==':' && getcmdline()=='h' ? 'tab help' : 'h'

nnoremap <leader>r :%s#<C-R><C-W>##g<Left><Left>
nnoremap <leader>R :%s#<C-R><C-A>##g<Left><Left>
vnoremap <leader>r "vy:%s#<C-R>v##g<Left><Left>
cnoremap <C-H> <Left>
cnoremap <C-L> <Right>

noremap <F2>  <ESC>:let @/ = expand('<cword>')\|set hlsearch<CR>
noremap <F3>  <ESC>:noh<CR>:echom 'Cancelled highlight'<CR>:redraw!<CR>
noremap <F4>  :s/^\(.\{-}\)\s*$/\1/g<CR><ESC>:noh<CR>:echom 'Trailing whitespaces removed'<CR>
noremap <F5>  :s/^\s*\(.\{-}\)\s*$/\1/g<CR><ESC>:noh<CR>:echom 'Leading and trailing whitespaces removed'<CR>
nnoremap <F6>  :exec 'silent !git difftool -y % &'<CR>:redraw!<CR>
nnoremap <C-F6>  :exec 'silent !gitk % &'<CR>:redraw!<CR>
noremap <F7>  <ESC><C-w>r<C-w>l<C-w>=
noremap <F8>  <ESC>:TagbarToggle<CR>
noremap <F9>  <ESC>:setlocal wrap!<CR>
" useful when one hand is holding the mouse
nmap <F10> <S-ENTER>
" noremap <F11>  <ESC>:GhcModTypeClear<CR>
noremap <F12> :%!python<CR>
" nnoremap <leader>2 :@x<CR>

" Movement
noremap <silent> <C-k> :wincmd k<CR>
noremap <silent> <C-j> :wincmd j<CR>
noremap <silent> <C-h> :wincmd h<CR>
noremap <silent> <C-l> :wincmd l<CR>

tnoremap <silent> <C-k> <C-\><C-n>:wincmd k<CR>
tnoremap <silent> <C-j> <C-\><C-n>:wincmd j<CR>
tnoremap <silent> <C-h> <C-\><C-n>:wincmd h<CR>
tnoremap <silent> <C-l> <C-\><C-n>:wincmd l<CR>

noremap <A-j> <ESC>:bn<CR>
noremap <A-k> <ESC>:bp<CR>
noremap <A-l> <ESC>:tabnext<CR>
noremap <A-h> <ESC>:tabprev<CR>
nnoremap <C-t> <ESC>:tabedit<CR>

tnoremap <A-j> <C-\><C-n>:bn<CR>
tnoremap <A-k> <C-\><C-n>:bp<CR>
tnoremap <A-l> <C-\><C-n>:tabnext<CR>
tnoremap <A-h> <C-\><C-n>:tabprev<CR>
tnoremap <C-t> <C-\><C-n>:tabedit<CR>

nnoremap <SPACE> <C-D>
nnoremap <S-SPACE> <C-U>
vnoremap <SPACE> <PageDown>
nnoremap <S-SPACE> <PageUp>
vnoremap <S-SPACE> <PageUp>
nnoremap j gj
vnoremap j gj
nnoremap k gk
vnoremap k gk

nnoremap <C-n> *

" Resize window
noremap <C-LEFT> <ESC>:vertical resize -2<CR>
noremap <C-RIGHT> <ESC>:vertical resize +2<CR>
noremap <C-UP> <ESC>:res +2<CR>
noremap <C-DOWN> <ESC>:res -2<CR>

" Navigation location list and qucikfix
nnoremap <DOWN> :lnext<CR>
nnoremap <UP> :lprevious<CR>
nnoremap <A-DOWN> :cnext<CR>
nnoremap <A-UP> :cprevious<CR>

" Spell checking
noremap <leader>sp :setlocal spell!<CR>

noremap <leader>hs :wincmd J<CR>
noremap <leader>vs :wincmd H<CR>

" Misc
noremap <A-d> <ESC>:bd!<CR>
noremap <A-w> <ESC>:wincmd c<CR>
map <DEL> gU

" Saving of files as sudo when I forgot to start vim using sudo.
cmap w!! w !sudo tee > /dev/null %

nnoremap <C-p> :Files<CR>
nnoremap <C-b> :Buffers<CR>
" }}}

" Folding {{{
set nofoldenable
set fdm=syntax

" Remove the underline and use Solarized Color Scheme light as background color
" highlight Folded gui=NONE guibg=#fdf6e3 " light
highlight Folded gui=NONE guibg=#002b36 " dark
set fillchars="vert:|,fold: "

" Toggle fold if there are any, otherwise do it supposed to do
noremap <expr> <2-LeftMouse> foldlevel(line('.'))==0 ? "\<2-LeftMouse>" : 'zA'
noremap <F1>  <ESC>zA:echom 'Toggle current fold recursively.'<CR>
" }}}

" Tabular {{{
vnoremap <silent> <leader>t :Tabular /
vnoremap <silent> <leader>t: :Tabular /:\zs<CR>
vnoremap <silent> <leader>t, :Tabular /,\zs<CR>
" AddTabularPattern comma_first_list_of_tuples /[[,]\zs\ /l0
" Enable some tabular presets for Haskell
let g:haskell_tabular = 1
" }}}

" Fzf {{{
let g:fzf_buffers_jump = 0
autocmd! FileType fzf tnoremap <buffer> <ESC> <c-c>
" }}}

" NerdTree {{{
noremap <A-n> :NERDTreeToggle<CR>
let g:NERDTreeMouseMode=2
" let g:NERDTreeChDirMode=0 " never auto change cwd for vim
" }}}

" Deoplete {{{
let g:deoplete#enable_at_startup=1
let g:deoplete#enable_smart_case=1
let g:deoplete#force_overwrite_completefunc=1
inoremap <expr><TAB> pumvisible() ? "\<C-n>" : "\<TAB>"

" Enable omni completion.
" autocmd FileType css           setlocal omnifunc=csscomplete#CompleteCSS
" autocmd FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
" autocmd FileType javascript    setlocal omnifunc=javascriptcomplete#CompleteJS
" autocmd FileType xml           setlocal omnifunc=xmlcomplete#CompleteTags
" autocmd FileType haskell       setlocal omnifunc=necoghc#omnifunc

" }}}

" NeoSnippet {{{
let g:neosnippet#snippets_directory = expand('~/.config/nvim/snippets/')
" SuperTab like snippets' behavior.
imap <expr><TAB> neosnippet#expandable_or_jumpable() ?
    \ "\<Plug>(neosnippet_expand_or_jump)"
    \ : pumvisible() ? "\<C-n>" : "\<TAB>"
"smap <expr><TAB> neosnippet#expandable_or_jumpable() ?
" \ "\<Plug>(neosnippet_expand_or_jump)"
" \: "\<TAB>"
" }}}

" Ag {{{
let g:ackprg = 'rg --vimgrep --follow --hidden --ignore-case --glob "!_build/*" --glob "!elm-stuff/*" --glob "!node_modules/*" --glob "!.git/*" --glob "!.svn/*" --glob "!**/.stack-work/*"'
cnoreabbrev <expr> ag getcmdtype()==':' && getcmdline()=='ag' ? 'Ack!' : 'ag'
" }}}

" tComment {{{
nmap <Leader>cc gcc
vmap <Leader>cc gc
nmap <Leader>ci :TCommentInline
vmap <Leader>ci :TCommentInline
au BufNewFile,BufRead *.proto setlocal commentstring=//%s
au BufNewFile,BufRead *.cabal setlocal commentstring=--%s
au BufNewFile,BufRead *.c setlocal commentstring=//%s
" }}}

" Session {{{
set ssop-=options " do not store global and local values in a session
set ssop-=folds   " do not store folds
set ssop-=help
set ssop-=buffers " don't save hidden and unloaded buffers
let g:session_persist_font = 0
let g:session_directory = expand('~/.config/nvim/sessions/')
let g:session_persist_colors = 0
let g:session_autosave = 'yes'
let g:session_autosave_periodic = 30
let g:session_verbose_messages = 0
let g:session_autoload = 'no'
cnoreabbrev <expr> os getcmdtype()==':' && getcmdline()=='os' ? 'OpenSession!' : 'os'
cnoreabbrev <expr> ss getcmdtype()==':' && getcmdline()=='ss' ? 'SaveSession'  : 'ss'
"}}}

" Remove Trailing Spaces {{{
autocmd FileType vim,haskell,python,css,html,dart,javascript,sql,markdown,elm,ocaml autocmd BufWritePre <buffer> :%s/\s\+$//e
" }}}

" Rename {{{
command! -nargs=* -complete=file -bang Rename :call Rename("<args>", "<bang>")
function! Rename(name, bang)
    let l:curfile = expand("%:p")
    let l:curfilepath = expand("%:p:h")
    let l:newname = l:curfilepath . "/" . a:name
    let v:errmsg = ""
    silent! exe "saveas" . a:bang . " " . l:newname
    if v:errmsg =~# '^$\|^E329'
        if expand("%:p") !=# l:curfile && filewritable(expand("%:p"))
            silent exe "bwipe! " . l:curfile
            if delete(l:curfile)
                echoerr "Could not delete " . l:curfile
            endif
        endif
    else
        echoerr v:errmsg
    endif
endfunction
" }}}

" Neomake and Autopep8 {{{
let g:neomake_erlang_enabled_makers = ['erlc']
let g:neomake_erlang_erlc_exe = 'erlang_check_file.erl'
let g:neomake_erlang_erlc_errorformat = '%W%f:%l: Warning: %m,' . '%E%f:%l: %m'

let g:neomake_python_enabled_makers = ['flake8']
" https://pycodestyle.readthedocs.io/en/latest/intro.html#error-codes
let py_ignored_errors='E501,E226,E261,E402,E305,E702,E302,E251,E503,F401,E116,E129,W503,E128,E123,E731,F811,E221,E303,E741,E222,E203,E266,E121,E126,E127,E241,E201,E125,E272,E114,E111'
let g:neomake_python_flake8_maker = {'args': ['--ignore=' . py_ignored_errors]}

let g:autopep8_ignore=py_ignored_errors
let g:autopep8_disable_show_diff=1

autocmd! BufWritePost * Neomake
" }}}

" Senter {{{
let g:senter_openconfig_jobsend_jupyter_console_command = 'source /opt/anaconda/bin/activate playground && jupyter console'
let g:senter_openconfig_jobsend_ghci_command = "stack ghci --ghci-options '+RTS -N2'"
" }}}

" Title String {{{
function! CurrentSession()
    return xolox#session#find_current_session()
endfunction

function! CurrentFileAndDirectory()
    let current_file = expand('%:t')
    let current_dir = fnamemodify(getcwd(), ':t')
    if current_file == ''
        return '∅ ' . current_dir
    else
        return current_file . ' ∈ ' . current_dir
endfunction

function! SetTitleString()
    let s = CurrentSession()
    let ts = (s != '') ? ('{' . s . '}') : CurrentFileAndDirectory()
    let &titlestring = ts
    return
endfunction

augroup l_dirchanged
    autocmd!
    autocmd DirChanged * :call SetTitleString()
augroup END

autocmd BufEnter * :call SetTitleString()
:call SetTitleString()
" }}}

" Customized Commands {{{
command! Gitk :call jobstart("gitk -- \'" . expand("%:p") . "\'")

command! -nargs=* T split | terminal <args>
command! -nargs=* Vt vsplit | terminal <args>
" }}}

" Easytags {{{
let g:easytags_auto_highlight = 0
let g:easytags_async = 1
let g:easytags_by_filetype = '~/.tags/'
let g:easytags_suppress_report = 1
" }}}

" WWW {{{
let g:www_engines = {
  \ 'oald' : 'https://en.oxforddictionaries.com/definition/{{QUERY}}',
  \ 'hoogle' : 'https://www.stackage.org/lts-9.2/hoogle?q={{QUERY}}',
  \ 'translate' : 'https://translate.google.com/#auto/en/{{QUERY}}' }

let g:www_default_search_engine = 'devdocs'

let g:www_shortcut_engines = {
  \ 'google': ['Google', '<leader>goo'],
  \ 'devdocs': ['Devdocs', '<leader>dd'],
  \ 'hoogle': ['Hoogle', '<leader>hoo'],
  \ 'translate': ['Translate', '<leader>tran'],
  \ 'oald': ['Oald', '<leader>ox'] }
" }}}

" Multi Cursor {{{
let g:multi_cursor_start_key='<C-m>'
let g:multi_cursor_next_key='<C-m>'
" let g:multi_cursor_exit_from_insert_mode = 0
" let g:multi_cursor_exit_from_visual_mode = 0

" Called once right before you start selecting multiple cursors
function! Multiple_cursors_before()
  call deoplete#init#_disable()
endfunc
function! Multiple_cursors_after()
  call deoplete#init#_enable()
endfunc
" }}}

" Far {{{
let g:far#preview_window_height = 30
let g:far#repl_devider = '  ➜  '
let far#highlight_match = 0
" }}}

" Statusline {{{
function! BracketNonempty(s) abort
    if a:s == ''
        return ''
    else
        return '[' . a:s . ']'
endfunction

set statusline=
set statusline+=%#identifier#%m%* " modified
set statusline+=%#identifier#%r%* " read only
set statusline+=%#identifier#%h%* " help
set statusline+=%#identifier#%w%* " preview

set statusline+=%#warningmsg#
set statusline+=%{&ff!='unix'?&ff:''}
set statusline+=%*

set statusline+=%#warningmsg#
set statusline+=%{(&fenc!='utf-8')&&(&fenc!='')?&fenc:''}
set statusline+=%*

set statusline+=%#warningmsg#
set statusline+=%{BracketNonempty(neomake#statusline#LoclistStatus())}
set statusline+=%*

set statusline+=%{BracketNonempty(CurrentSession())}
set statusline+=%f " file name

set statusline+=%{BracketNonempty(SenterStatusLine())}

set statusline+=%=

set statusline+=%{fugitive#statusline()}

set statusline+=\ C%c\ L%l/%L\ %P
" }}}

" File Manipulation {{{

function! CopyFile() abort
python3 << EOF
import vim
import shutil

def input(prompt, default_value):
    vim.command('call inputsave()')
    vim.command(f"let user_input = input('{prompt}: ', '{default_value}')")
    vim.command('call inputrestore()')
    return vim.eval('user_input')

currentPath = vim.current.buffer.name
destPath = input('Enter new file path', currentPath)
shutil.copy(currentPath, destPath)
print(f'File copied to {destPath}')
EOF
endfunction
command! CopyFile call CopyFile()

" }}}

" EasyMotion {{{
hi link EasyMotionTarget helpHyperTextJump
nmap f <Plug>(easymotion-s)
"}}}"

" LSP {{{
let g:LanguageClient_serverCommands = {
    \ 'haskell': ['hie-wrapper', '--lsp'] }

nnoremap <F11> :call LanguageClient_contextMenu()<CR>
map <Leader>lk :call LanguageClient#textDocument_hover()<CR>
map <Leader>lg :call LanguageClient#textDocument_definition()<CR>
map <Leader>lr :call LanguageClient#textDocument_rename()<CR>
map <Leader>lf :call LanguageClient#textDocument_formatting()<CR>
map <Leader>lb :call LanguageClient#textDocument_references()<CR>
map <Leader>la :call LanguageClient#textDocument_codeAction()<CR>
map <Leader>ls :call LanguageClient#textDocument_documentSymbol()<CR>

"}}}

" OCaml {{{
" ## added by OPAM user-setup for vim / base ## 93ee63e278bdfc07d1139a748ed3fff2 ## you can edit, but keep this line
let s:opam_share_dir = system("opam config var share")
let s:opam_share_dir = substitute(s:opam_share_dir, '[\r\n]*$', '', '')

let s:opam_configuration = {}

function! OpamConfOcpIndent()
  execute "set rtp^=" . s:opam_share_dir . "/ocp-indent/vim"
endfunction
let s:opam_configuration['ocp-indent'] = function('OpamConfOcpIndent')

function! OpamConfOcpIndex()
  execute "set rtp+=" . s:opam_share_dir . "/ocp-index/vim"
endfunction
let s:opam_configuration['ocp-index'] = function('OpamConfOcpIndex')

function! OpamConfMerlin()
  let l:dir = s:opam_share_dir . "/merlin/vim"
  execute "set rtp+=" . l:dir
endfunction
let s:opam_configuration['merlin'] = function('OpamConfMerlin')

let s:opam_packages = ["ocp-indent", "ocp-index", "merlin"]
let s:opam_check_cmdline = ["opam list --installed --short --safe --color=never"] + s:opam_packages
let s:opam_available_tools = split(system(join(s:opam_check_cmdline)))
for tool in s:opam_packages
  " Respect package order (merlin should be after ocp-index)
  if count(s:opam_available_tools, tool) > 0
    call s:opam_configuration[tool]()
  endif
endfor
" ## end of OPAM user-setup addition for vim / base ## keep this line

" ## added by OPAM user-setup for vim / ocp-indent ## f52b5826eb7d55a633f4864dd670870e ## you can edit, but keep this line
" if count(s:opam_available_tools,"ocp-indent") == 0
"   source "/home/incomplete/.opam/4.04.2/share/vim/syntax/ocp-indent.vim"
" endif
" ## end of OPAM user-setup addition for vim / ocp-indent ## keep this line

nnoremap <A-t> :MerlinTypeOf<CR>
vnoremap <A-t> :MerlinTypeOf<CR>

" }}}

" Haskell {{{
let g:haskell_indent_in = 0
let g:haskell_indent_before_where = 0
let g:haskell_indent_after_bare_where = 2
let g:haskell_indent_if = 2
"}}}

" Elm vim {{{
let g:elm_format_autosave = 0
" }}}

" Erlang {{{
autocmd FileType erlang setlocal iskeyword+=:
" }}}

" Python {{{
autocmd Filetype python setlocal shiftwidth=2 tabstop=2 softtabstop=2
" }}}

" HTML {{{
" Treat .mythcss as CSS file, and auto compile it
autocmd BufNewFile,BufRead,BufReadPost *.mythcss setfiletype css
autocmd BufWritePost *.mythcss silent execute '!myth --compress'.' '.shellescape(expand('%:p')).' '.shellescape(expand('%:r').'.css')
autocmd BufNewFile,BufRead,BufReadPost *.djhtml setfiletype htmldjango
autocmd BufNewFile,BufRead,BufReadPost *.eliom setfiletype ocaml
autocmd BufNewFile,BufRead,BufReadPost *.eliomi setfiletype ocaml

autocmd FileType qml,javascript setlocal tabstop=4 softtabstop=4 shiftwidth=4
" }}}

" Indent Line {{{
let g:indentLine_char = '┆'
" let g:indentLine_color_gui = '#F5E9CA' " light
let g:indentLine_color_gui = '#073642	' " dark
let g:indentLine_concealcursor = ''
" }}}

" Jedi {{{
let g:jedi#completions_enabled = 0
let g:jedi#rename_command = ''
autocmd FileType python setlocal completeopt-=preview
" }}}

" vim:fdm=marker
" vim:foldenable
