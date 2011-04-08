Udon
====

Functional programming has a number of common, idiomatic operations: maps,
folds, currying and so on. There are a number of excellent JavaScript libraries
which provide access to these fundamentals in a more-or-less functional style.
[Ojay]'s core extensions, Oliver Steele's [Functional] and Jeremy Ashkenas'
[Underscore] are all good examples.

[Udon] is cast in a similar mould to the above. It aims to provide basic
support for functional programming in JavaScript, initially by porting some
elements of Haskell's `Data.List` library.

[Udon]:       http://extralogical.net/projects/udon/
[Ojay]:       http://ojay.othermedia.org/
[Underscore]: http://documentcloud.github.com/underscore/
[Functional]: http://osteele.com/sources/javascript/functional/


Building the library
--------------------

If you've checked out the Git repository and want to build a minified version
of the library from source, you will need [Ruby], [Rubygems] and the [Jake]
gem, which you can get by running (with `sudo` if you like)

    gem install jake

from the command line. Then just `cd` into your Udon directory and run

    jake

which will build the library in the `/pkg` directory.

[Ruby]:     http://ruby-lang.org
[Rubygems]: http://rubygems.org
[Jake]:     https://github.com/jcoglan/jake


Running the test suite
----------------------

The Udon test suite is written with the [JS.Test] testing framework. The test
suite runs on several JavaScript platforms, including [Node] and all modern web
browsers.

To run the test suite from the command line, run the following command
(replacing `node` with [Rhino], [V8] etc. where relevant).

    node test/console.js

To run the test suite in a browser, open the `test/browser.html` file in a web
browser.

[JS.Test]: http://jsclass.jcoglan.com/testing.html
[Node]:    http://nodejs.org
[Rhino]:   http://www.mozilla.org/rhino/
[V8]:      http://code.google.com/p/v8/


Building the Udon website
-------------------------

The website is built with [Hakyll], a static site generator written in
[Haskell]. To build it you will need [GHC] and [cabal-install]; the simplest
way to get hold of these is to install the [Haskell Platform]. You'll also need
to install the `hakyll` package from [Hackage]:

    cabal install hakyll

Then, to build the site, just run this from your Udon directory:

    ghc --make -Wall site/site

[Hakyll]:           http://jaspervdj.be/hakyll/
[Haskell]:          http://www.haskell.org
[GHC]:              http://www.haskell.org/ghc/
[cabal-install]:    http://www.haskell.org/cabal/download.html
[Haskell Platform]: http://hackage.haskell.org/platform/
[Hackage]:          http://hackage.haskell.org/
