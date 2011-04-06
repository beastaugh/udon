Udon
====

Functional programming has a number of common, idiomatic operations: maps,
folds, currying and so on. There are a number of excellent JavaScript libraries
which provide access to these fundamentals in a more-or-less functional style.
[Ojay][ojay]'s core extensions, Oliver Steele's [Functional][functionaljs] and
Jeremy Ashkenas' [Underscore][underscore] are all good examples.

[Udon][udon] is cast in a similar mould to the above. It aims to provide basic
support for functional programming in JavaScript, initially by porting some
elements of Haskell's `Data.List` library.

[udon]:  http://extralogical.net/projects/udon/


Function operations
-------------------

Passing functions around as first-class objects is one of the cornerstones of
functional programming. Udon provides several handy mechanisms for enabling
function composition and reuse.

### `curry`

Currying is the process of converting a function of arity _n_ into a nested set
of functions with arity _1_, i.e. making it partially applicable. The `curry`
function relies on the length property of the function to generate partially
applicable functions, for example converting a function which accepts two
arguments to a function which accepts one argument and returns a new function
which also accepts one argument.

    function add(a, b) {
        return a + b;
    }
    
    var plus10 = Udon.curry(add)(10);

### `ncurry`

The basic `curry` function will be fine for many circumstances, but sometimes
(for example when dealing with variadic functions or functions with optional
arguments) one needs to be explicit about the number of arguments a curried
function can accept. The `ncurry` function is a generator for currying
functions: it accepts a number and returns a curry function that transforms
functions of that arity to, effectively, a nest of partially applicable
functions, each of which has arity 1.

    function add3(a, b, c) {
        return a + b + c;
    }
    
    var curry3 = Udon.ncurry(3),
        add3c  = curry3(add3);
    
    add3c(1)(2)(3); // -> 6

### `compose`

The `compose` function allows one to easily generate 'pipelines' of functions
through which a value is passed. Note that the last function in the pipeline
will be the first to be applied; this mirrors both the way the code would be
written without `compose`, as a nest of function calls.

    var tcs = Udon.compose([Math.sin, Math.cos, Math.tan]);
    
    tcs(0.7); // -> 0.6176546934901699

It accepts an optional arity argument; if this is greater than 1 then the
function pipeline will be made partially applicable.
    
    var ceilMax = Udon.compose([Math.ceil, Math.max], 2);
    
    roundMax(0.7)(1.1); // -> 2


List operations
---------------

Technically, these aren't really list operations, because the underlying data
structures are JavaScript arrays, not singly-linked lists (as in Lisps, ML,
Haskell etc.). They are, however, close enough for most practical purposes.

### `foldl`

Both fold functions accept a function and use it to reduce a list to another
value. For example, you could use it to implement a `sum` function which adds
all the elements of a list together:

    function sum(ns) {
        return Udon.foldl(function(a, b) {
            return a + b;
        }, 0, ns);
    }

### `foldr`

As the name implies, `foldl` is a left-associative function, which `foldr` is
right-associative. So, for example, you could use `foldr` to convert an array
into a singly-linked list.

    function array2list(arr) {
        return Udon.foldr(function(head, tail) {
            return {
                car: head,
                cdr: tail
            };
        }, {car: null, cdr: null}, arr);
    }

You can read more about folds [on Wikipedia][fold].

### `map`

Returns the result of applying a given function to each element of a list.

    Udon.map(function(n) {
        return n * n;
    }, [1,2,3]);
    // -> [1,4,9]

### `filter`

Returns the elements of a list which satisfy some predicate.

    Udon.filter(function(n) {
        return n < 5;
    }, [4,7,3,9,21,2]);
    // -> [4,3,2]

### `any`

Check whether any element of a list satisfies some predicate.

    Udon.any(function(regex) {
        return regex.exec("http://");
    }, [/[a-z]+:\/\//, /^ftp:/]);
    // -> true

### `all`

Determine whether all the elements of a list satisfy some predicate.

    Udon.all(function(str) {
        return str.match(/^[A-Z][a-z]+$/);
    }, ["One", "Two", "three"]);
    // -> false

### `none`

Check that no element of a list satisfies a predicate.

    Udon.none(function(c) {
        return c === Math.PI;
    }, [1, 0, -1, Math.LN2, Math.E]);
    // -> true

### `partition`

Separates a list into lists of those elements which do and do not satisfy some
predicate.

    Udon.partition(function(n) {
        return n < 5;
    }, [4,7,3,9,21,2]);
    // -> [[4,3,2], [7,9,21]]

### `unfoldr`

Builds a list by repeatedly applying a function to a seed value. The function
should return a pair of values: the first is an element to append to the list
under construction, while the second is the seed value to pass to the next
function call. The function must return `null` when it's done, at which point
`unfoldr` will return the constructed list.

    function randomInts(ceil, n) {
        return Udon.unfoldr(function(i) {
            return i < 1 ? null : [Math.floor(Math.random() * ceil), i - 1];
        }, n);
    }
    
    randomInts(5, 4); // -> [5,1,4,5] e.g.

### `zip`

Transforms a pair of lists into a list of pairs. If the lists are of differing
lengths, any elements left over after each element in the shorter list has been
paired up with one from the longer list will be discarded.

    Udon.zip([1,2,3], ["a", "b", "c"]); // -> [[1, "a"], [2, "b"], [3, "c"]]

### `zipWith`

The `zipWith` function is a generalisation of `zip`: it returns the result of
applying a function to each pair of elements from two lists.

    Udon.zipWith(function(a, b) {
        return a * b;
    }, [1,2,3], [4,5,6]);
    // -> [4,10,18]


[ojay]:         http://ojay.othermedia.org/
[underscore]:   http://documentcloud.github.com/underscore/
[functionaljs]: http://osteele.com/sources/javascript/functional/
[fold]:         http://en.wikipedia.org/wiki/Fold_(higher-order_function)
