" Global settings
set laststatus=2
set t_Co=256
syntax on
map <C-n> :NERDTreeToggle<CR>

" Machine-specific settings
let opsys = system('uname -s')
let host = system('uname -n')
if (opsys=~"Darwin") 
   let $PYTHONPATH='/usr/local/lib/python3.8/site-packages'
   set rtp+=/usr/local/lib/python3.8/site-packages/powerline/bindings/vim
endif
if (opsys=~"Linux")
   if (host=~"schopenhauer")
      let $PYTHONPATH="$HOME/.local/lib/python2.7/site-packages"
      set rtp+=$HOME/.local/lib/python2.7/site-packages/powerline/bindings/vim
   endif
endif
