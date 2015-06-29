" Vundle {{{
    set nocompatible
    filetype off

    set rtp+=~/.vim/bundle/Vundle.vim
    call vundle#begin()

    Plugin 'gmarik/Vundle.vim'
    Plugin 'Shougo/neocomplete.vim'
    Plugin 'Shougo/neosnippet'
    Plugin 'Shougo/neosnippet-snippets'
    Plugin 'Shougo/vimproc.vim'
    Plugin 'Twinside/vim-hoogle'
    Plugin 'airblade/vim-rooter'
    Plugin 'altercation/vim-colors-solarized'
    Plugin 'dart-lang/dart-vim-plugin'
    Plugin 'dkprice/vim-easygrep'
    Plugin 'eagletmt/ghcmod-vim'
    Plugin 'eagletmt/neco-ghc'
    Plugin 'fishcakez/vim-rebar'
    Plugin 'godlygeek/tabular'
    Plugin 'gorkunov/smartpairs.vim'
    Plugin 'kien/ctrlp.vim'
    Plugin 'majutsushi/tagbar'
    Plugin 'othree/xml.vim'
    Plugin 'scrooloose/syntastic'
    Plugin 'tmhedberg/matchit'
    Plugin 'tomtom/tcomment_vim'
    Plugin 'tpope/vim-fugitive'
    Plugin 'tpope/vim-pathogen'
    Plugin 'tpope/vim-surround'
    Plugin 'vim-erlang/vim-erlang-runtime'
    Plugin 'vim-erlang/vim-erlang-tags'
    Plugin 'xolox/vim-easytags'
    Plugin 'xolox/vim-misc'
    Plugin 'xolox/vim-session'
    Plugin 'scrooloose/nerdtree'
    Plugin 'mileszs/ack.vim'
    Plugin 'thinca/vim-ref'
    Plugin 'henrik/vim-indexed-search'
    Plugin 'vim-scripts/drawit'
    Plugin 'fmoralesc/vim-pad'

    call vundle#end()
    filetype plugin on
    filetype indent on
" }}}

" VIM Options {{{
    let $PATH.=':'.expand('~/bin')
    let $PATH.=':'.expand('~/codes/ghc-mod/dist/7.10.1/build/ghc-mod/')
    let $PATH.=':'.expand('~/codes/ghc-mod/dist/7.10.1/build/ghc-modi/')
    scriptencoding utf-8
    set nocompatible

    " GUI stuff
    if has('gui_running')
        set background=light
        set guioptions-=m
        set guioptions-=T
        set guioptions-=t
        set guioptions+=b
        set guicursor+=a:blinkon0 " do not blink cursor
        set guifont=Input\ Mono\ 10
        colorscheme solarized
    else
        set background=dark
    endif

    let mapleader=","
    syntax on

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
    set history=1000
    set laststatus=2
    set showcmd

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

    " When 'wrap' is on, use '↳ ' in the next line to indicate a wrap
    highlight! link NonText Character
    exec "set showbreak=↳"
    set showbreak+=\ 

    set tags=tags,codex.tags,~/codes/otp_src_17.5/tags
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

    inoremap ;; ::
    inoremap -- ->

    " Pair
    inoremap '' ''<ESC>i
    inoremap "" ""<ESC>i
    inoremap <> <><ESC>i
    inoremap () ()<ESC>i
    inoremap [] []<ESC>i
    inoremap {} {}<ESC>i

    nnoremap ; :
    vnoremap ; :
    nnoremap : ;
    vnoremap : ;

    nnoremap <A-,> ^
    nnoremap <A-.> $
    nnoremap Y y$

    vnoremap <C-Insert> "+y
    nnoremap <S-Insert> "+p

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

    noremap <F3>  <ESC>:noh<CR>:echom 'Cancelled highlight'<CR>:redraw!<CR>
    noremap <F4>  :s/^\(.\{-}\)\s*$/\1/g<CR><ESC>:noh<CR>:echom 'Trailing whitespaces removed'<CR>
    noremap <F5>  :s/^\s*\(.\{-}\)\s*$/\1/g<CR><ESC>:noh<CR>:echom 'Leading and trailing whitespaces removed'<CR>
    noremap <F8>  :TagbarToggle<CR>
    noremap <F9>  <ESC>:setlocal wrap!<CR>
    noremap <F12>  <ESC>:w<CR>:!start cmd /c python "%" & pause<CR>
    noremap <C-F12> :!python<CR>

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
    noremap <C-LEFT> <ESC>:vertical resize -1<CR>
    noremap <C-RIGHT> <ESC>:vertical resize +1<CR>
    noremap <C-UP> <ESC>:res +1<CR>
    noremap <C-DOWN> <ESC>:res -1<CR>

    " Spell checking
    noremap <leader>sp :setlocal spell!<CR>

    " Misc
    noremap <A-d> <ESC>:bd!<CR>
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
    let g:ctrlp_switch_buffer = 'h'

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

" NerdTree {{{
    noremap <A-n> :NERDTreeToggle<CR>
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

" NeoSnippet {{{
    let g:neosnippet#snippets_directory = expand('~/.vim/snippets/')
    " SuperTab like snippets' behavior.
    imap <expr><TAB> neosnippet#expandable_or_jumpable() ?
        \ "\<Plug>(neosnippet_expand_or_jump)"
        \ : pumvisible() ? "\<C-n>" : "\<TAB>"
    "smap <expr><TAB> neosnippet#expandable_or_jumpable() ?
    " \ "\<Plug>(neosnippet_expand_or_jump)"
    " \: "\<TAB>"
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

    let g:syntastic_sh_checkers=['shellcheck']

    let g:syntastic_mode_map =
    \ { "mode": "active"
    \ , "passive_filetypes": ["dart"]
    \ }

    noremap <leader>sc <ESC>:SyntasticCheck<CR>
" }}}

" Rooter {{{
    let g:rooter_autocmd_patterns = '*.hs,*.dart'
    let g:rooter_use_lcd = 1
" }}}

" Easytags {{{
    let g:easytags_auto_highlight=0
    let g:easytags_auto_update = 0
    let g:easytags_autorecurse = 1


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

" Ag {{{
let g:ackprg = 'ag --nogroup --nocolor --column --ignore=tags'
cnoreabbrev <expr> ag getcmdtype()==':' && getcmdline()=='ag' ? 'Ack' : 'ag'
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

" VimPad {{{
    let g:pad#dir = "~/Notes"
" }}}

" For These File Types {{{
    autocmd FileType haskell,python,css,html,dart,javascript,sql autocmd BufWritePre <buffer> :%s/\s\+$//e
" }}}

" Haskell {{{
    " Show types in completion suggestions
    let g:necoghc_enable_detailed_browse = 1

    " Hoogle
    let g:hoogle_search_count = 15
    let g:hoogle_search_buffer_size = 15
    cnoreabbrev <expr> hoogle getcmdtype()==':' && getcmdline()=='hoogle' ? 'Hoogle' : 'hoogle'
    cnoreabbrev <expr> hoogleinfo getcmdtype()==':' && getcmdline()=='hoogleinfo' ? 'HoogleInfo' : 'hoogleinfo'
    nnoremap <Leader>hh :Hoogle<CR>
    nnoremap <Leader>hi :HoogleInfo<CR>
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

" Erlang {{{
autocmd FileType erlang setlocal iskeyword+=:
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

    set statusline+=%r
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

    set statusline+=\ C%c     " Cursor column
    set statusline+=\ L%l/%L  " Cursor line/total lines

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

" Google it {{{
    function! s:google()
      let url = 'https://www.google.com/search?q='
      let q = substitute(
            \ ''.@0.'',
            \ '[^A-Za-z0-9_.~-]',
            \ '\="%".printf("%02X", char2nr(submatch(0)))',
            \ 'g')
      call system('xdg-open ' . url . q)
    endfunction
    xnoremap <leader>? y:call <SID>google()<CR>
" }}}

" Work {{{
    function! GenSuperWingsErlc()
        return "!erlc "
           \ . "-pa ~/codes/SuperWingsServer/deps/lager/ebin "
           \ . "-pa ~/codes/SuperWingsServer/deps/dynarec/ebin "
           \ . "-I ~/codes/SuperWingsServer/include "
           \ . "-I ~/codes/SuperWingsServer/deps/ "
           \ . "-o ~/codes/SuperWingsServer/ebin "
           \ . "-Wall "
           \ . "+'{parse_transform, lager_transform}' +'{lager_print_records_flag, false}' "
           \ . "+debug_info "
           \ . "+bin_opt_info "
           \ . expand('%:p')
    endfunction
    cnoreabbrev <expr> csw getcmdtype()==':' && getcmdline()=='csw' ? GenSuperWingsErlc() : 'csw'
" }}}

" vim:fdm=marker
" vim:foldenable
