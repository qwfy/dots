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
    Plug 'xolox/vim-easytags'

    Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
    Plug 'Shougo/neosnippet'
    Plug 'Shougo/neosnippet-snippets'

    Plug 'tomtom/tcomment_vim'

    Plug 'tpope/vim-fugitive'
    Plug 'scrooloose/nerdtree'

    Plug 'godlygeek/tabular'
    Plug 'gorkunov/smartpairs.vim'    " viv
    Plug 'henrik/vim-indexed-search'    " match M of N
    Plug 'majutsushi/tagbar'
    Plug 'thinca/vim-ref'
    Plug 'tmhedberg/matchit'    " use % to match more things

    Plug 'qwfy/vim-slime'

    " languages
    Plug 'elmcast/elm-vim'

    Plug 'eagletmt/neco-ghc'
    Plug 'neovimhaskell/haskell-vim'

    Plug 'hynek/vim-python-pep8-indent'
    Plug 'zchee/deoplete-jedi'

    Plug 'brooth/far.vim' " find and replace

    Plug 'waiting-for-dev/vim-www'

    " Plug 'vim-multiple-cursors'

    call plug#end()

" }}}

" Vim Options {{{
    let $PATH.=':'.expand('~/bin')
    let $PATH.=':'.expand('~/bin/nodejs/bin')
    let $PATH.=':'.expand('~/.local/bin')
    scriptencoding utf-8
    filetype plugin on
    filetype indent on
    syntax on

    set background=light
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
    set swapfile
    set backupdir=/tmp//,.

    set number
    set incsearch
    set hlsearch
    set ic
    set history=10000
    set laststatus=2
    set showcmd
    set wildignorecase

    set conceallevel=2
    set concealcursor=nc
    hi clear Conceal

    " Space and indent
    set expandtab
    set tabstop=4
    set softtabstop=4
    set shiftwidth=4
    set smarttab
    set backspace=indent,eol,start
    set smartindent
    set autoindent

    " Long lines
    set breakindent
    " Indent the wrapped line

    set nowrap
    " When 'wrap' is off, use '»' as the last column to indicate a wrap
    set listchars=extends:»

    highlight! link NonText Character
    set showbreak=↳

    set termguicolors
" }}}

" Key Bindings {{{
    " C-S
    noremap  <C-S> :update<CR>
    vnoremap <C-S> <C-C>:update<CR>
    inoremap <C-S> <C-O>:update<CR>

    " Abbr
    iabbrev todo: TODO incomplete:
    iabbrev fixme: FIXME incomplete:

    inoremap .. ->
    inoremap ... ..
    inoremap ,, <-
    inoremap >> =>
    inoremap >>> >>=

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
    inoremap gul «
    inoremap gur »
    inoremap gup «»<ESC>i

    nnoremap ; :
    vnoremap ; :
    nnoremap : ;
    vnoremap : ;

    nnoremap <A-,> ^
    nnoremap <A-.> $
    nnoremap Y y$

    " temporary, see https://github.com/equalsraf/neovim-qt/issues/215
    vnoremap <C-Insert> "+y
    nnoremap <S-Insert> "+p
    inoremap <S-Insert> <ESC>"+pa

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
    noremap <F8>  :TagbarToggle<CR>
    noremap <F9>  <ESC>:setlocal wrap!<CR>
    noremap <F10>  <ESC>:GhcModType<CR>
    noremap <F11>  <ESC>:GhcModTypeClear<CR>
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

    " Spell checking
    noremap <leader>sp :setlocal spell!<CR>

    " Misc
    noremap <A-d> <ESC>:bd!<CR>
    noremap <A-w> <ESC>:wincmd c<CR>
    map <DEL> gU

    " Saving of files as sudo when I forgot to start vim using sudo.
    cmap w!! w !sudo tee > /dev/null %

    nnoremap <C-p> :Files<CR>
    autocmd! FileType fzf tnoremap <buffer> <ESC> <c-c>
" }}}

" Folding {{{
    set nofoldenable
    set fdm=indent
    set foldnestmax=1

    " Remove the underline and use Solarized Color Scheme light as background color
    highlight Folded gui=NONE guibg=#FDF6E3

    " Toggle fold if there are any, otherwise do it supposed to do
    noremap <expr> <2-LeftMouse> foldlevel(line('.'))==0 ? "\<2-LeftMouse>" : 'zA'
    noremap <F1>  <ESC>zA:echom 'Toggle current fold recursively.'<CR>
" }}}

" Tabular {{{
    vnoremap <silent> <leader>t :Tabular /
    vnoremap <silent> <leader>t: :Tabular /:\zs<CR>
    " AddTabularPattern comma_first_list_of_tuples /[[,]\zs\ /l0
    " Enable some tabular presets for Haskell
    let g:haskell_tabular = 1
" }}}

" Fzf {{{
    let g:fzf_buffers_jump = 0
" }}}

" NerdTree {{{
    noremap <A-n> :NERDTreeToggle<CR>
    let g:NERDTreeMouseMode=2 " single click to open
    " let g:NERDTreeChDirMode=0 " never auto change cwd for vim
" }}}

" Deoplete {{{
    let g:deoplete#enable_at_startup=1
    let g:deoplete#enable_smart_case=1
    let g:deoplete#force_overwrite_completefunc=1
    inoremap <expr><TAB> pumvisible() ? "\<C-n>" : "\<TAB>"

    " Enable omni completion.
    autocmd FileType css           setlocal omnifunc=csscomplete#CompleteCSS
    autocmd FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
    autocmd FileType javascript    setlocal omnifunc=javascriptcomplete#CompleteJS
    autocmd FileType xml           setlocal omnifunc=xmlcomplete#CompleteTags
    autocmd FileType haskell       setlocal omnifunc=necoghc#omnifunc

    " Close the scratch window which is shown when working with python file
    set completeopt-=preview
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
    let g:ackprg = 'rg --vimgrep --hidden --ignore-case --glob "!_build/*" --glob "!elm-stuff/*" --glob "!node_modules/*" --glob "!.git/*" --glob "!.svn/*"'
    cnoreabbrev <expr> ag getcmdtype()==':' && getcmdline()=='ag' ? 'Ack!' : 'ag'
" }}}

" tComment {{{
    nmap <Leader>cc gcc
    vmap <Leader>cc gc
    nmap <Leader>ci :TCommentInline
    vmap <Leader>ci :TCommentInline
    au BufNewFile,BufRead *.proto setlocal commentstring=//%s
    au BufNewFile,BufRead *.cabal setlocal commentstring=--%s
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

" Erlang {{{
    autocmd FileType erlang setlocal iskeyword+=:
" }}}

" HTML {{{
    " Treat .mythcss as CSS file, and auto compile it
    autocmd BufNewFile,BufRead,BufReadPost *.mythcss setfiletype css
    autocmd BufWritePost *.mythcss silent execute '!myth --compress'.' '.shellescape(expand('%:p')).' '.shellescape(expand('%:r').'.css')
    autocmd BufNewFile,BufRead,BufReadPost *.djhtml setfiletype htmldjango
    autocmd BufNewFile,BufRead,BufReadPost *.eliom setfiletype ocaml
    autocmd BufNewFile,BufRead,BufReadPost *.eliomi setfiletype ocaml

    " Use two spaces as indent for these file types
    autocmd FileType html,css,dart,ocaml setlocal tabstop=2 softtabstop=2 shiftwidth=2
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

" Neomake {{{
    let g:neomake_erlang_enabled_makers = ['erlc']
    let g:neomake_erlang_erlc_exe = 'erlang_check_file.erl'
    let g:neomake_erlang_erlc_errorformat = '%W%f:%l: Warning: %m,' . '%E%f:%l: %m'
    autocmd! BufWritePost * Neomake
" }}}

" Slime {{{
    let g:slime_target="neovim"
    xmap <S-ENTER> <Plug>SlimeRegionSend
    nmap <S-ENTER> <Plug>SlimeParagraphSend
    let g:slime_python_ipython=1
" }}}

" Title String {{{
    function! CurrentSession()
        return xolox#session#find_current_session()
    endfunction

    function! CurrentSessionStatusline()
        let s = CurrentSession()
        return (s != '') ? ('[' . s . ']' ) : ('')
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
if count(s:opam_available_tools,"ocp-indent") == 0
  source "/home/incomplete/.opam/4.04.2/share/vim/syntax/ocp-indent.vim"
endif
" ## end of OPAM user-setup addition for vim / ocp-indent ## keep this line


nnoremap <A-t> :MerlinTypeOf<CR>
vnoremap <A-t> :MerlinTypeOf<CR>

" }}}

" Elm vim {{{
    let g:elm_format_autosave = 0
" }}}

" Customized Commands {{{
    command Gitk :call jobstart("gitk -- \'" . expand("%:p") . "\'")

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
      \ 'translate' : 'https://translate.google.com/#auto/en/{{QUERY}}'
      \ }

    let g:www_default_search_engine = 'devdocs'

    let g:www_shortcut_engines = {
      \ 'google': ['Google', '<leader>goo'],
      \ 'devdocs': ['Devdocs', '<leader>dd'],
      \ 'hoogle': ['Hoogle', '<leader>hoo'],
      \ 'translate': ['Translate', '<leader>tran'],
      \ 'oald': ['Oald', '<leader>ox']
    \}

" }}}

" Multi Cursor {{{
    " let g:multi_cursor_start_key='<C-m>'
    " let g:multi_cursor_next_key = '<C-m>'
    " let g:multi_cursor_exit_from_insert_mode = 0
    " let g:multi_cursor_exit_from_visual_mode = 0
" }}}

" Far {{{
    let g:far#preview_window_height = 30
    let g:far#repl_devider = '  ➜  '
    let far#highlight_match = 0
" }}}

" Statusline {{{
    set statusline=

    " modified flag
    set statusline+=%#identifier#
    set statusline+=%m
    set statusline+=%*

    " read only flag
    set statusline+=%#identifier#
    set statusline+=%r
    set statusline+=%*

    " warn if the file format is not UNIX
    set statusline+=%#warningmsg#
    set statusline+=%{&ff!='unix'?'['.&ff.']':''}
    set statusline+=%*

    " warn if the file encoding is not UTF-8
    set statusline+=%#warningmsg#
    set statusline+=%{(&fenc!='utf-8'&&&fenc!='')?'['.&fenc.']':''}
    set statusline+=%*

    " neomake
    set statusline+=%#warningmsg#
    set statusline+=%{NeomakeStatus()}
    set statusline+=%*

    " help file flag
    set statusline+=%h

    set statusline+=%f

    " left/right separator
    set statusline+=%=

    " git status
    set statusline+=%{CurrentSessionStatusline()}
    set statusline+=%{fugitive#statusline()}

    " columns lines and percentage
    set statusline+=\ C%c
    set statusline+=\ L%l/%L
    set statusline+=\ %P

    function! NeomakeStatus()
        let s = neomake#statusline#LoclistStatus()
        return (s!='')?(s):('')
    endfunction

" }}}

" vim:fdm=marker
" vim:foldenable
