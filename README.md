Udon: practical functional programming in JavaScript
====================================================

Functional programming has a number of common, idiomatic operations---maps,
folds, currying and so on. There are a number of excellent JavaScript libraries
which provide access to these fundamentals in a more-or-less functional style.
[Ojay][ojay]'s core extensions, Oliver Steele's [Functional][functionaljs] and
the [Underscore][underscore] "utility-belt library" are all good examples.

Udon is cast in a similar mould to the above. It aims to provide basic support
for pure(ish) functional programming in JavaScript. The driving principles
behind the library's creation are pragmatic: interoperating on different
platforms; using the language's existing primitives, particularly arrays; and
providing only those operations which the author has found invaluable in
everyday programming, both in browsers and on the server.

[ojay]:         http://ojay.othermedia.org/
[underscore]:   http://documentcloud.github.com/underscore/
[functionaljs]: http://osteele.com/sources/javascript/functional/
