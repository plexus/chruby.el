# chruby.el

Emacs support for Chruby. It works identical to the shell command, after placing `chruby.el` in your load-path you can

````elisp
(require 'chruby)
(chruby "jruby-1.7.2")
````

To run it from within emacs, you have to start and eval prompt using `M-:` (the Meta and colon keys), and then type `(chruby "jruby-1.7.2")` for example.

In fact just like Chruby it is extremely lightweight, it merely follows the same conventions for finding rubies on your system.

Some of the code was taken from [rvm.el](https://github.com/senny/rvm.el) by Yves Senn, however any violations of good style are entirely mine.

There is currently no support for Chruby's 'auto' functionality.

Copyright 2013-2014 Arne Brasseur

For license details refer to the LICENSE file.
