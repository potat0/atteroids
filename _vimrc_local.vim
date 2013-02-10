let s:current_file=expand("<sfile>:p:h")
exe 'chdir' . s:current_file
set makeprg=cabal\ build
