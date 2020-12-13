" Global settings
set laststatus=2
set t_Co=256
syntax on
map <C-n> :NERDTreeToggle<CR>

" Machine-specific settings
let opsys = system('uname -s')
if (opsys=~"Darwin") 
   let $PYTHONPATH='/usr/local/lib/python3.8/site-packages'
   set rtp+=/usr/local/lib/python3.8/site-packages/powerline/bindings/vim
endif
