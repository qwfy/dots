" Plug {{{
    call plug#begin('~/.local/share/nvim/plugged')

    Plug 'equalsraf/neovim-gui-shim'
    Plug 'frankier/neovim-colors-solarized-truecolor-only'

    Plug 'kien/ctrlp.vim'
    Plug 'mileszs/ack.vim'
    Plug 'neomake/neomake'

    Plug 'xolox/vim-misc'
    Plug 'xolox/vim-session'

    Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
    Plug 'Shougo/neosnippet'
    Plug 'Shougo/neosnippet-snippets'

    Plug 'tpope/vim-fugitive'

    Plug 'sjl/gundo.vim'
    Plug 'godlygeek/tabular'
    Plug 'gorkunov/smartpairs.vim'      " viv
    Plug 'henrik/vim-indexed-search'    " match M of N
    Plug 'majutsushi/tagbar'
    Plug 'scrooloose/nerdtree'
    Plug 'thinca/vim-ref'
    Plug 'tmhedberg/matchit'    " use % to match more things

    Plug 'tomtom/tcomment_vim'
    Plug 'xolox/vim-easytags'

    " languages
    Plug 'elmcast/elm-vim'
    Plug 'vim-erlang/vim-erlang-tags'

    Plug 'eagletmt/neco-ghc'
    Plug 'itchyny/vim-haskell-indent'

    Plug 'hynek/vim-python-pep8-indent'
    Plug 'davidhalter/jedi-vim'

    call plug#end()

" }}}

" Vim Options {{{
    let $PATH.=':'.expand('~/bin')
    let $PATH.=':'.expand('~/.local/bin')
    let $PATH.=':'.expand('~/bin/erlang/18.3/bin')
    scriptencoding utf-8
    filetype plugin on
    filetype indent on
    syntax on

    set background=light
    colorscheme solarized

    let mapleader=","

    " File related
    set fileformat=unix
    set fileformats=unix,dos
    set fileencoding=utf-8
    set fileencodings=utf-8,gbk
    set encoding=utf-8
    set autoread
    set noswapfile
    set backupdir=/tmp//,.

    set number
    set incsearch
    set hlsearch
    set ignorecase
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

    set tags=tags,~/code/otp_src_18.2.1/tags,~/.python_tags
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
    inoremap ... ...
    inoremap -> ..
    inoremap << <-
    " inoremap >> <<>>

    inoremap ;; ::
    inoremap ,, =>
    inoremap \\ (\)


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

    nnoremap ; :
    vnoremap ; :
    nnoremap : ;
    vnoremap : ;

    nnoremap <A-,> ^
    nnoremap <A-.> $
    nnoremap Y y$

    vnoremap <C-S-C> "+y
    nnoremap <C-S-V> "+p
    inoremap <C-S-V> <ESC>"+pa

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

    " Movement
    noremap <silent> <C-k> :wincmd k<CR>
    noremap <silent> <C-j> :wincmd j<CR>
    noremap <silent> <C-h> :wincmd h<CR>
    noremap <silent> <C-l> :wincmd l<CR>
    noremap <A-j> <ESC>:bn<CR>
    noremap <A-k> <ESC>:bp<CR>
    noremap <A-l> <ESC>:tabnext<CR>
    noremap <A-h> <ESC>:tabprev<CR>
    nnoremap <C-t> <ESC>:tabedit<CR>
    nnoremap <SPACE> <PageDown>
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
    map <DEL> ~

    " Saving of files as sudo when I forgot to start vim using sudo.
    cmap w!! w !sudo tee > /dev/null %
" }}}

" Folding {{{
    " Don't fold by default
    set nofoldenable

    " Remove the underline and use Solarized Color Scheme light as background color
    highlight Folded gui=NONE guibg=#FDF6E3

    " Toggle fold if there are any, otherwise do it supposed to do
    noremap <expr> <2-LeftMouse> foldlevel(line('.'))==0 ? "\<2-LeftMouse>" : 'zA'
    noremap <F1>  <ESC>zA:echom 'Toggle current fold recursively.'<CR>
" }}}

" Tabular {{{
    vnoremap <silent> <leader>t :Tabular /
    vnoremap <silent> <leader>t: :Tabular /:\zs<CR>
    " Enable some tabular presets for Haskell
    let g:haskell_tabular = 1
" }}}

" CtrlP {{{
    let g:ctrlp_open_multiple_files='rv'
    let g:ctrlp_open_new_file = 'r'
    let g:ctrlp_switch_buffer = 'h'

    let g:ctrlp_clear_cache_on_exit=0
    let g:ctrlp_working_path_mode='a'
    let g:ctrlp_use_caching=1
    let g:ctrlp_regexp=1
    let g:ctrlp_show_hidden=0
    let g:ctrlp_custom_ignore={
        \ 'dir'  : '\v(\.git|\.hg|\.svn|packages|build|Mnesia\.node.*|data.run)$',
        \ 'file' : '\v(\.hi|\.o|\.jpg|\.jpeg|\.bmp\.png\.exe|\.so|\.dll|\.beam|\.pyc|\~|\.xlsx\.docx)$',
        \ 'link' : '\vpackages$'
        \ }
    noremap <Leader>m <Esc>:CtrlPMRU<CR>
" }}}

" NerdTree {{{
    noremap <A-n> :NERDTreeToggle<CR>
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
    autocmd FileType python        setlocal omnifunc=jedi#completions
    autocmd FileType xml           setlocal omnifunc=xmlcomplete#CompleteTags
    autocmd FileType haskell       setlocal omnifunc=necoghc#omnifunc

    let g:jedi#completions_enabled = 0
    let g:jedi#auto_vim_configuration = 0
    if !exists('g:deoplete#force_omni_input_patterns')
            let g:deoplete#force_omni_input_patterns = {}
    endif
    let g:deoplete#force_omni_input_patterns.python = '\%([^. \t]\.\|^\s*@\|^\s*from\s.\+import \|^\s*from \|^\s*import \)\w*'

    " Close the scratch window which is shown when working with python file
    " set completeopt-=preview
    " Close the preview window when completion is done
    " au CompleteDone * pclose
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

" Rooter {{{
    " let g:rooter_autocmd_patterns = '*.hs,*.dart'
    " let g:rooter_use_lcd = 1
" }}}

" Ag {{{
    let g:ackprg = 'ag --nogroup --nocolor --column --ignore=tags --skip-vcs-ignores --ignore-case'
    cnoreabbrev <expr> ag getcmdtype()==':' && getcmdline()=='ag' ? 'Ack' : 'ag'
" }}}

" tComment {{{
    nmap <Leader>cc gcc
    vmap <Leader>cc gc
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
    autocmd FileType haskell,python,css,html,dart,javascript,sql,markdown autocmd BufWritePre <buffer> :%s/\s\+$//e
" }}}

" Erlang {{{
    autocmd FileType erlang setlocal iskeyword+=:
    let g:erlang_tags_ignore = ["_build"]
" }}}

" HTML {{{
    " Treat .mythcss as CSS file, and auto compile it
    autocmd BufNewFile,BufRead,BufReadPost *.mythcss setfiletype css
    autocmd BufWritePost *.mythcss silent execute '!myth --compress'.' '.shellescape(expand('%:p')).' '.shellescape(expand('%:r').'.css')
    autocmd BufNewFile,BufRead,BufReadPost *.djhtml setfiletype htmldjango

    " Use two spaces as indent for these file types
    autocmd FileType html,css,dart setlocal tabstop=2 softtabstop=2 shiftwidth=2
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
    set statusline+=%{fugitive#statusline()}

    " columns lines and percentage
    set statusline+=\ C%c
    set statusline+=\ L%l/%L
    set statusline+=\ %P

    function! NeomakeStatus()
        let s = neomake#statusline#LoclistStatus()
        return (s!='')?('['.s.']'):('')
    endfunction

" }}}

" vim:fdm=marker
" vim:foldenable
