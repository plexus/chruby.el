# chruby.el

Emacs support for Chruby. It works identical to the shell command, after placing `chruby.el` in your load-path you can

````elisp
(require 'chruby)
(chruby "jruby-1.7.2")
````

Interactively, either call `M-x chruby-use` which will prompt for a ruby version, or `M-x chruby-use-corresponding` which tries to get the version from a `.ruby-version` file.

In fact just like Chruby it is extremely lightweight, it merely follows the same conventions for finding rubies on your system.

Some of the code was taken from [rvm.el](https://github.com/senny/rvm.el) by Yves Senn, however any violations of good style are entirely mine.

There is currently no support for Chruby's 'auto' functionality.

Copyright 2013-2014 Arne Brasseur

For license details refer to the LICENSE file.
