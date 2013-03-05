# chruby.el

Emacs support for Chruby. It works identical to the shell command, after placing `chruby.el` in your load-path you can

````elisp
(require 'chruby)
(chruby "jruby-1.7.2")
````

In fact just like Chruby it is extremely lightweight, it merely follows the same conventions for finding rubies on your system.

Some of the code was taken from [rvm.el](https://github.com/senny/rvm.el) by Yves Senn, however any violations of good style are entirely mine.

There is currently no support for Chruby's 'auto' functionality.