" Vundle {{{
    set nocompatible
    filetype off

    set rtp+=~/.vim/bundle/Vundle.vim
    call vundle#begin()

    Plugin 'gmarik/Vundle.vim.git'
    Plugin 'Shougo/neocomplete.vim.git'
    Plugin 'Shougo/vimproc.vim.git'
    Plugin 'Twinside/vim-hoogle.git'
    Plugin 'airblade/vim-rooter.git'
    Plugin 'altercation/vim-colors-solarized.git'
    Plugin 'dart-lang/dart-vim-plugin.git'
    Plugin 'dkprice/vim-easygrep.git'
    Plugin 'eagletmt/ghcmod-vim.git'
    Plugin 'eagletmt/neco-ghc.git'
    Plugin 'fishcakez/vim-rebar.git'
    Plugin 'godlygeek/tabular.git'
    Plugin 'gorkunov/smartpairs.vim.git'
    Plugin 'kien/ctrlp.vim.git'
    Plugin 'majutsushi/tagbar'
    Plugin 'othree/xml.vim.git'
    Plugin 'scrooloose/syntastic.git'
    Plugin 'tmhedberg/matchit.git'
    Plugin 'tomtom/tcomment_vim.git'
    Plugin 'tpope/vim-fugitive.git'
    Plugin 'tpope/vim-pathogen.git'
    Plugin 'tpope/vim-surround.git'
    Plugin 'vim-erlang/vim-erlang-runtime.git'
    Plugin 'vim-erlang/vim-erlang-tags.git'
    Plugin 'xolox/vim-easytags.git'
    Plugin 'xolox/vim-misc.git'
    Plugin 'xolox/vim-session.git'

    call vundle#end()
    filetype plugin on
    filetype indent on
" }}}

" VIM Options {{{
    scriptencoding utf-8
    set nocompatible

    " GUI stuff
    set shortmess+=I
    colorscheme solarized
    if has('gui_running')
        set vb t_vb=
        set background=light
        set guioptions-=m
        set guioptions-=T
        set guioptions-=t
        set guioptions+=b
        set guicursor+=a:blinkon0 " do not blink cursor
        set guifont=Input\ Mono\ 10
    else
        set background=dark
    endif

    let mapleader=","

    " File related
    set fileformat=unix
    set fileformats=unix,dos
    set encoding=utf-8
    set nobackup
    set noswapfile
    set noundofile
    set autoread

    set number
    set incsearch
    set hlsearch
    set ignorecase
    set history=1000
    set laststatus=2

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
    set nowrap
    " When 'wrap' is off, use '»' as the last column to indicate a wrap
    set listchars=extends:»
    " Indent the wrapped line
    set breakindent
    " When 'wrap' is on, use '» ' in the next line to indicate a wrap
    exec "set showbreak=\u00BB"
    set showbreak=\ 
" }}}

" Folding {{{
    set foldmethod=syntax

    " Don't fold by default
    set nofoldenable

    " Remove the underline and use Solarized Color Scheme light as background color
    highlight Folded gui=NONE guibg=#FDF6E3

    " Toggle fold if there are any, otherwise do it supposed to do
    noremap <expr> <2-LeftMouse> foldlevel(line('.'))==0 ? "\<2-LeftMouse>" : 'zA'
    noremap <F1>  <ESC>zA:echom 'Toggle current fold recursively.'<CR>
" }}}

" Key Bindings {{{
    " C-S, C-C, C-V
    noremap  <C-S> :update<CR>
    vnoremap <C-S> <C-C>:update<CR>
    inoremap <C-S> <C-O>:update<CR>

    " Abbr
    iabbrev todo TODO
    iabbrev fixme FIXME

    iabbrev ;; ::
    iabbrev -- ->

    " Pair
    inoremap '' ''<ESC>i
    inoremap "" ""<ESC>i
    inoremap <> <><ESC>i
    inoremap () ()<ESC>i
    inoremap [] []<ESC>i
    inoremap {} {}<ESC>i

    noremap ` <ESC>mqgEwi'<ESC>Ea'<ESC>`ql<ESC>:delmark q<CR>:echom 'Quote WORD with single quotes'<CR>
    noremap `` <ESC>mqgEwi"<ESC>Ea"<ESC>`ql<ESC>:delmark q<CR>:echom 'Quote WORD with double quotes'<CR>
    noremap ``` <ESC>mqgEwr'<ESC>Er'<ESC>`q<ESC>:delmark q<CR>:echom 'Change double quotes to single quotes'<CR>
    noremap ```` <ESC>mqgEwr"<ESC>Er"<ESC>`q<ESC>:delmark q<CR>:echom 'Change single quotes to double quotes'<CR>
    noremap `1 <ESC>mqgEwx<ESC>Ex<ESC>`qh<ESC>:delmark q<CR>:echom 'Remove quotes'<CR>

    nnoremap ; :
    vnoremap ; :
    nnoremap : ;
    vnoremap : ;

    nnoremap H ^
    nnoremap L $

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

    noremap <F3>  <ESC>:noh<CR>:echom 'Cancelled highlight'<CR>
    noremap <F4>  :s/^\(.\{-}\)\s*$/\1/g<CR><ESC>:noh<CR>:echom 'Trailing whitespaces removed'<CR>
    noremap <F5>  :s/^\s*\(.\{-}\)\s*$/\1/g<CR><ESC>:noh<CR>:echom 'Leading and trailing whitespaces removed'<CR>
    noremap <F8>  :TagbarToggle<CR>
    noremap <F9>  <ESC>:set wrap!<CR>
    noremap <F12>  <ESC>:w<CR>:!start cmd /c python "%" & pause<CR>
    noremap <C-F12> :!python<CR>

    " Movement
    noremap <silent> <A-k> :wincmd k<CR>
    noremap <silent> <A-j> :wincmd j<CR>
    noremap <silent> <A-h> :wincmd h<CR>
    noremap <silent> <A-l> :wincmd l<CR>
    noremap <C-j> <ESC>:bn<CR>
    noremap <C-k> <ESC>:bp<CR>
    noremap <C-l> <ESC>:tabnext<CR>
    noremap <C-h> <ESC>:tabprev<CR>
    noremap <C-t> <ESC>:tabedit<CR>
    nnoremap <SPACE> <PageDown>
    vnoremap <SPACE> <PageDown>
    nnoremap <S-SPACE> <PageUp>
    vnoremap <S-SPACE> <PageUp>
    nnoremap j gj
    vnoremap j gj
    nnoremap k gk
    vnoremap k gk

    " Resize window
    noremap <C-LEFT> <ESC>:vertical resize -1<CR>
    noremap <C-RIGHT> <ESC>:vertical resize +1<CR>
    noremap <C-UP> <ESC>:res +1<CR>
    noremap <C-DOWN> <ESC>:res -1<CR>

    " Spell checking
    noremap <leader>ss :setlocal spell!<CR>
    noremap <leader>sn ]s
    noremap <leader>sp [s
    noremap <leader>sa zg
    noremap <leader>s? z=

    " Misc
    noremap <C-d> <ESC>:bd!<CR>
    map <DEL> ~
" }}}

" Tabular {{{
    vnoremap <silent> <leader>t :Tabular /
    " Enable some tabular presets for Haskell
    let g:haskell_tabular = 1
" }}}

" CtrlP {{{
    let g:ctrlp_open_multiple_files='rv'
    let g:ctrlp_open_new_file = 'r'
    "
    " e: <cr> to jump to window in current tab if exist, otherwise open new
    " T: <c-t> jump to window anyware if exist, otherwise open new
    let g:ctrlp_switch_buffer = 'eT'

    let g:ctrlp_clear_cache_on_exit=0
    let g:ctrlp_working_path_mode='a'
    let g:ctrlp_use_caching=1
    let g:ctrlp_regexp=1
    let g:ctrlp_show_hidden=0
    let g:ctrlp_custom_ignore={
        \ 'dir'  : '\v(\.git|\.hg|\.svn|packages|build|Mnesia\.node.*)$',
        \ 'file' : '\v(\.hi|\.o|\.jpg|\.jpeg|\.bmp\.png\.exe|\.so|\.dll|\.beam|\.pyc|\~)$',
        \ 'link' : '\vpackages$'
        \ }
    noremap <Leader>m <Esc>:CtrlPMRU<CR>
" }}}

" NeoComplete {{{
    let g:neocomplete#enable_at_startup=1
    let g:neocomplete#enable_smart_case=1
    let g:neocomplete#force_overwrite_completefunc=1
    inoremap <expr><TAB> pumvisible() ? "\<C-n>" : "\<TAB>"

    " Enable omni completion.
    autocmd FileType css           setlocal omnifunc=csscomplete#CompleteCSS
    autocmd FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
    autocmd FileType javascript    setlocal omnifunc=javascriptcomplete#CompleteJS
    autocmd FileType python        setlocal omnifunc=pythoncomplete#Complete
    autocmd FileType xml           setlocal omnifunc=xmlcomplete#CompleteTags
" }}}

" Syntastic {{{
    let g:syntastic_always_populate_loc_list = 1

    let g:syntastic_python_checkers=['pylint']
    let g:syntastic_python_pylint_args='
    \ -E
    \ --disable=C0111,C0112,C0301,C0103,C0326,
    \           W0603,W0614,W0401,W0703,W0702,W1202,
    \           R0903
    \ '

    let g:syntastic_haskell_checkers=['hlint']
    let g:syntastic_haskell_hlint_args='--ignore="Use camelCase"'

    let g:syntastic_css_checkers=[]
    let g:syntastic_css_csslint_options='
    \ --ignore=ids,
    \          unqualified-attributes,
    \          universal-selector,
    \          regex-selectors,
    \          compatible-vendor-prefixes
    \ '

    let g:syntastic_mode_map =
    \ { "mode": "active"
    \ , "passive_filetypes": ["dart", "haskell"]
    \ }

    noremap <leader>sc <ESC>:SyntasticCheck<CR>
" }}}

" Rooter {{{
    let g:rooter_autocmd_patterns = '*.hs,*.dart'
    let g:rooter_use_lcd = 1
" }}}

" Easytags {{{
    set tags=tags;/,codex.tags;/
    let g:easytags_auto_highlight=0

    let g:easytags_languages =
    \ {'haskell':
    \   { 'cmd': 'hasktags'
    \   , 'args': []
    \   , 'fileoutput_opt': '-f'
    \   , 'stdout_opt': '-f-'
    \   , 'recurse_flag': ''
    \   }
    \ }
" }}}

" EasyGrep {{{
    let g:EasyGrepFilesToExclude=".git,.pub,packages,__pycache__"
    nnoremap <leader>go <ESC>:GrepOptions<CR>
" }}}

" tComment {{{
    nmap <Leader>cc gcc
    vmap <Leader>cc gc
" }}}

" Session {{{
    set ssop-=options " do not store global and local values in a session
    set ssop-=folds   " do not store folds
    let g:session_autosave = 'yes'
    let g:session_autosave_periodic = 30
    let g:session_verbose_messages = 0
    let g:session_autoload = 'no'
    cnoreabbrev <expr> os getcmdtype()==':' && getcmdline()=='os' ? 'OpenSession!' : 'os'
    cnoreabbrev <expr> ss getcmdtype()==':' && getcmdline()=='ss' ? 'SaveSession'  : 'ss'
"}}}

" For These File Types {{{
    autocmd FileType haskell,python,css,html,dart,javascript,sql autocmd BufWritePre <buffer> :%s/\s\+$//e
" }}}

" Haskell {{{
    " Show types in completion suggestions
    let g:necoghc_enable_detailed_browse = 1

    " Hoogle
    let g:hoogle_search_count = 15
    let g:hoogle_search_buffer_size = 15
    nnoremap <Leader>hh :Hoogle
    nnoremap <Leader>hH :Hoogle<CR>
    nnoremap <Leader>hi :HoogleInfo
    nnoremap <Leader>hI :HoogleInfo<CR>
    nnoremap <Leader>hc :HoogleClose<CR>

    " Tagbar
    let g:tagbar_type_haskell =
    \  { 'ctagsbin': 'hasktags'
    \  , 'ctagsargs': '-x -c -o-'
    \  , 'sro': '.'
    \  , 'kinds':
    \      [ 'm:modules:0:1'
    \      , 'd:data: 0:1'
    \      , 'd_gadt: data gadt:0:1'
    \      , 't:type names:0:1'
    \      , 'nt:new types:0:1'
    \      , 'c:classes:0:1'
    \      , 'cons:constructors:1:1'
    \      , 'c_gadt:constructor gadt:1:1'
    \      , 'c_a:constructor accessors:1:1'
    \      , 'ft:function types:1:1'
    \      , 'fi:function implementations:0:1'
    \      , 'o:others:0:1'
    \      ]
    \  , 'kind2scope':
    \      { 'm': 'module'
    \      , 'c': 'class'
    \      , 'd': 'data'
    \      , 't': 'type'
    \      }
    \  , 'scope2kind':
    \      { 'module': 'm'
    \      , 'class': 'c'
    \      , 'data': 'd'
    \      , 'type': 't'
    \      }
    \  }

" }}}

" Python {{{
    autocmd! FileType python setlocal nosmartindent
" }}}

" HTML Development {{{
    " Treat .mythcss as CSS file, and auto compile it
    autocmd BufNewFile,BufRead,BufReadPost *.mythcss setfiletype css
    autocmd BufWritePost *.mythcss silent execute '!myth --compress'.' '.shellescape(expand('%:p')).' '.shellescape(expand('%:r').'.css')

    " Html funky comment to remove spaces between inline-blocks
    vmap <leader>hf :s/^\(\s*\)</\1+--></g<CR>gv:s/>$/><!--+/g<CR><ESC>:noh<CR>
    vmap <Leader>huf :s/\(+-->\\|<!--+\)//g<CR>:noh<CR>

    " Use two spaces as indent for these file types
    autocmd FileType html,css,dart set tabstop=2 softtabstop=2 shiftwidth=2
" }}}

" Statusline {{{
    set laststatus=2

    " Modified flag
    set statusline+=%#identifier#
    set statusline+=%m
    set statusline+=%*

    " Read only flag
    set statusline+=%#identifier#
    set statusline+=%r
    set statusline+=%*

    " Warn if the file format is not UNIX
    set statusline+=%#warningmsg#
    set statusline+=%{&ff!='unix'?'['.&ff.']':''}
    set statusline+=%*

    " Warn if the file encoding is not UTF-8
    set statusline+=%#warningmsg#
    set statusline+=%{(&fenc!='utf-8'&&&fenc!='')?'['.&fenc.']':''}
    set statusline+=%*

    set statusline+=%#warningmsg#
    set statusline+=%{StatuslineTabWarning()}
    set statusline+=%*
    set statusline+=%#warningmsg#
    set statusline+=%{StatuslineTrailingSpaceWarning()}
    set statusline+=%*

    set statusline+=%#warningmsg#
    set statusline+=%{SyntasticStatuslineFlag()}
    set statusline+=%*

    " Help file flag
    set statusline+=%h

    set statusline+=%f

    set statusline+=%= " Left/right separator

    set statusline+=%{StatuslineTagbar()}
    set statusline+=%{fugitive#statusline()}

    set statusline+=\ %c     " Cursor column
    set statusline+=\ %l/%L  " Cursor line/total lines

    set statusline+=\ %{StatuslineWindowDim()}

    set statusline+=\ %P     " Percent through file

    " Return the editable dimension of the window
    " FIXME The returned width considers only sign column, line number column and
    "       the editing area. If other special columns are present, the returned
    "       width will be strictly greater than the actual width.
    function! StatuslineWindowDim()
      let ww=winwidth(0)
      if &number
        let number_column_width = max([len(line('$')), &numberwidth-1]) + 1
        let ww -= number_column_width
      endif
      if has('signs')
        " If the sign column is shown, substract the width of sign column,
        " that is always 2, from ww
        redir =>sc_temp
          exe 'sil sign place buffer='.bufnr('%')
        redir end
        let sc_list = split(sc_temp, '\n')
        " If there are signs, then `sil sign place ...` will output:
        " --- Signs ---
        " Signs for <filename>
        " <list of signs>
        let sign_column_width = len(sc_list)>2 ? 2 : 0
        let ww -= sign_column_width
      endif

      return ww . 'x' . winheight(0)
    endfunction

    function! StatuslineTrailingSpaceWarning()
      if !exists("b:statusline_trailing_space_warning")
        let b:statusline_trailing_space_warning = ''
        let trailing = search('\s$', 'nw')
        if trailing != 0
          let b:statusline_trailing_space_warning ='[trailing ' . trailing . ']'
        endif
      endif
      return b:statusline_trailing_space_warning
    endfunction
    autocmd cursorhold,bufwritepost * unlet! b:statusline_trailing_space_warning

    function! StatuslineTabWarning()
      if !exists("b:statusline_tab_warning")
        let b:statusline_tab_warning = ''
        let tabs = search('^\t', 'nw')
        let spaces = search('^ \{' . &ts . ',}[^\t]', 'nw')

        if (tabs != 0) && (spaces != 0)
          let b:statusline_tab_warning = '[mixed-indenting ' . spaces . '] '
        elseif (spaces && !&expandtab) || (tabs && &expandtab)
          let b:statusline_tab_warning ='[expandtab ' . tabs . '] '
        endif
      endif
      return b:statusline_tab_warning
    endfunction
    autocmd cursorhold,bufwritepost * unlet! b:statusline_tab_warning

    function! StatuslineTagbar()
      let s = ''
      let tag = tagbar#currenttag('%s','')
      let s = tag!='' ? '[' . tag . ']' : ''
      return s
    endfunction

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

" vim:fdm=marker
" vim:foldenable
