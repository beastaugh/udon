---
title: Udon | Practical functional programming in JavaScript
---

[Udon] is a library providing basic support for functional programming idioms
in JavaScript.

[Udon]: https://github.com/beastaugh/udon


Downloads
---------

Current version: **1.2.0**.

* [Development version](./downloads/udon-1.2.0.js)
* [Production version](./downloads/udon-1.2.0-min.js)
  4.5kb packed, 1.0kb gzipped

This version adds the following new functions to the library: `id`, `concat`,
`sum`, `product`, `elem`, `notElem`, `reverse`, `intersperse` and
`intercalate`.


Programming with Udon
---------------------

All the functions in this library are namespaced by the `Udon` object, so a
reference in the documentation to e.g. `map` should be read as `Udon.map`. This
means it should play nicely with other JavaScript libraries.

Udon is known to work in all major browsers, as well as on [Node]. As well as
being available from the download links above, you can install it with [npm].

    npm install -g udon

The Node library exports the Udon object as the module, so usage should follow
this pattern.

~~~{.JavaScript}
var Udon = require('udon');

Udon.zip([1, 2, 3], ['a', 'b', 'c']);
// -> [[1, 'a'], [2, 'b'], [3, 'c']]
~~~

Udon is designed for pure functional programming, so none of its public
functions (i.e. those listed below) modify the values passed to them.

[Node]: http://nodejs.org
[npm]:  http://npmjs.org

API summary
-----------

* [Function operations](#api-function-operations)
    - [`curry`](#api-curry)
    - [`ncurry`](#api-ncurry)
    - [`compose`](#api-compose)
    - [`id`](#api-id)
* [List operations](#api-list-operations)
    - [`foldl`](#api-foldl)
    - [`foldl1`](#api-foldl1)
    - [`foldr`](#api-foldr)
    - [`concat`](#api-concat)
    - [`maximum`](#api-maximum)
    - [`minimum`](#api-minimum)
    - [`maximumBy`](#api-maximumBy)
    - [`minimumBy`](#api-minimumBy)
    - [`map`](#api-map)
    - [`reverse`](#api-reverse)
    - [`intersperse`](#api-intersperse)
    - [`intercalate`](#api-intercalate)
    - [`filter`](#api-filter)
    - [`any`](#api-any)
    - [`all`](#api-all)
    - [`none`](#api-none)
    - [`sum`](#api-sum)
    - [`product`](#api-product)
    - [`elem`](#api-elem)
    - [`notElem`](#api-notElem)
    - [`partition`](#api-partition)
    - [`unfoldr`](#api-unfoldr)
    - [`zip`](#api-zip)
    - [`zipWith`](#api-zipwith)


<h2 id="api-function-operations">Function operations</h2>

Passing functions around as first-class objects is one of the cornerstones of
functional programming. Udon provides several handy mechanisms for enabling
function composition and reuse.

<h3 id="api-curry"><code>curry</code></h3>

Currying is the process of converting a function of arity _n_ into a nested set
of functions with arity _1_, i.e. making it partially applicable. The `curry`
function relies on the length property of the function to generate partially
applicable functions, for example converting a function which accepts two
arguments to a function which accepts one argument and returns a new function
which also accepts one argument.

~~~{.JavaScript}
var add = function(a, b) {
    return a + b;
};

var plus10 = Udon.curry(add)(10);
~~~

<h3 id="api-ncurry"><code>ncurry</code></h3>

The basic `curry` function will be fine for many circumstances, but sometimes
(for example when dealing with variadic functions or functions with optional
arguments) one needs to be explicit about the number of arguments a curried
function can accept. The `ncurry` function is a generator for currying
functions: it accepts a number and returns a curry function that transforms
functions of that arity to, effectively, a nest of partially applicable
functions, each of which has arity 1.

~~~{.JavaScript}
var add3 = function(a, b, c) {
    return a + b + c;
};

var curry3 = Udon.ncurry(3),
    add3c  = curry3(add3);

add3c(1)(2)(3); // -> 6
~~~

<h3 id="api-compose"><code>compose</code></h3>

The `compose` function allows one to easily generate 'pipelines' of functions
through which a value is passed. Note that the last function in the pipeline
will be the first to be applied; this mirrors both the way the code would be
written without `compose`, as a nest of function calls.

~~~{.JavaScript}
var tcs = Udon.compose([Math.sin, Math.cos, Math.tan]);

tcs(0.7); // -> 0.6176546934901699
~~~

It accepts an optional arity argument; if this is greater than 1 then the
function pipeline will be made partially applicable.
    
~~~{.JavaScript}
var ceilMax = Udon.compose([Math.ceil, Math.max], 2);

ceilMax(0.7)(1.1); // -> 2
~~~


<h3 id="api-id"><code>id</code></h3>

It's occasionally useful to be able to pass a function that simply returns its
argument to a higher-order function. `id` is that function.

~~~{.JavaScript}
Udon.id(5) == 5;
Udon.id("foobar") == "foobar";
~~~

Passing `id` to `map` produces a nice fixed point, as we can see from the
following example where `xs0`, `xs1` and `xs2` are all equivalent.

~~~{.JavaScript}
var xs0 = [1, 4, 9],
    xs1 = Udon.id([1, 4, 9]),
    xs2 = Udon.map(Udon.id, [1, 4, 9]);
~~~


<h2 id="api-list-operations">List operations</h2>

Technically, these aren't really list operations, because the underlying data
structures are JavaScript arrays, not singly-linked lists (as in Lisps, ML,
Haskell etc.). They are, however, close enough for most practical purposes.

<h3 id="api-foldl"><code>foldl</code></h3>

Both fold functions accept a function and use it to reduce a list to another
value. For example, you could use it to implement a `sum` function which adds
all the elements of a list together:

~~~{.JavaScript}
var sum = function(ns) {
    return Udon.foldl(function(a, b) {
        return a + b;
    }, 0, ns);
};
~~~

<h3 id="api-foldl1"><code>foldl1</code></h3>

This is a version of `foldl` which uses the first element of the supplied array
as the initial element. It thus requires the array to be nonempty.

~~~{.JavaScript}
var join = function(a, b) {
    return a.toString() + '-' + b.toString();
};

Udon.foldl1(join, [2011, 05, 08]);
// -> "2011-05-08"
~~~

<h3 id="api-foldr"><code>foldr</code></h3>

As the name implies, `foldl` is a left-associative function, which `foldr` is
right-associative. So, for example, you could use `foldr` to convert an array
into a singly-linked list.

~~~{.JavaScript}
var array2list = function(arr) {
    return Udon.foldr(function(head, tail) {
        return {
            car: head,
            cdr: tail
        };
    }, {car: null, cdr: null}, arr);
};
~~~

You can read more about folds [on Wikipedia][fold].

[fold]: http://en.wikipedia.org/wiki/Fold_(higher-order_function)

<h3 id="api-concat"><code>concat</code></h3>

Flattens an array one level.

~~~{.JavaScript}
Udon.concat([[1, 2], [3, 4]]);
// [1, 2, 3, 4]
Udon.concat([[[1, 2], [3, 4]], [[5, 6], [7, 8]]]);
// [[1, 2], [3, 4], [5, 6], [7, 8]]
~~~

<h3 id="api-maximum"><code>maximum</code></h3>

Selects the largest number from an array. It uses `Math.max` under the hood so
it won't work for e.g. strings. Use [`Udon.maximumBy`](#api-maximumBy) if you
need your own comparison function.

~~~{.JavaScript}
Udon.maximum([1, 3, 2, 17, 12]);
// -> 17
~~~

<h3 id="api-minimum"><code>minimum</code></h3>

Selects the smallest number from an array. It uses `Math.min` under the hood so
it won't work for e.g. strings. Use [`Udon.minimumBy`](#api-minimumBy) if you
need your own comparison function.

~~~{.JavaScript}
Udon.minimum([1, 3, 2, 17, 12]);
// -> 1
~~~

<h3 id="api-maximumBy"><code>maximumBy</code></h3>

Uses a given comparison function to select the largest element of an array.
Comparison functions should take two arguments, and return `0` if they are
considered equal, `1` if the first argument is greater than the second, and
`-1` if the second argument is greater than the first.

~~~{.JavaScript}
var longerThan = function(x, y) {
    if (x.length === y.length) {
        return 0;
    } else if (x.length > y.length) {
        return 1;
    } else {
        return -1;
    }
};

Udon.maximumBy(longerThan, ["foo", "foobar", "foobarbaz"]);
// -> "foobarbaz"
~~~

<h3 id="api-minimumBy"><code>minimumBy</code></h3>

Uses a given comparison function to select the smallest element of an array.
Comparison functions should operate the same way as those given to
[`maximumBy`](#api-maximumBy).

~~~{.JavaScript}
Udon.minimumBy(longerThan, ["foo", "foobar", "foobarbaz"]);
// -> "foo"
~~~

<h3 id="api-map"><code>map</code></h3>

Returns the result of applying a given function to each element of a list.

~~~{.JavaScript}
Udon.map(function(n) {
    return n * n;
}, [1,2,3]);
// -> [1,4,9]
~~~

<h3 id="api-reverse"><code>reverse</code></h3>

The `reverse` function provides a safe alternative to the [`reverse` method],
as it does not mutate the input array.

~~~{.JavaScript}
Udon.reverse(['a', 'b', 'c', 'd']);
// -> ['d', 'c', 'b', 'a']
~~~

<h3 id="api-intersperse"><code>intersperse</code></h3>

Insert a separating element between the existing elements, like the `join`
method does but without converting the array to a string.

~~~{.JavaScript}
Udon.intersperse(10, [1, 2, 3, 4]);
// -> [1, 10, 2, 10, 3, 10, 4]
~~~

<h3 id="api-intercalate"><code>intercalate</code></h3>

Effectively `intersperse` for arrays of arrays, but flattening the resultant
array one level.

~~~{.JavaScript}
Udon.intercalate([1, 2, 3], [[10, 20], [30, 40], [50, 60]]);
// -> [10, 20, 1, 2, 3, 30, 40, 1, 2, 3, 50, 60]
~~~

<h3 id="api-filter"><code>filter</code></h3>

Returns the elements of a list which satisfy some predicate.

~~~{.JavaScript}
Udon.filter(function(n) {
    return n < 5;
}, [4,7,3,9,21,2]);
// -> [4,3,2]
~~~

<h3 id="api-any"><code>any</code></h3>

Check whether any element of a list satisfies some predicate.

~~~{.JavaScript}
Udon.any(function(regex) {
    return regex.exec("http://");
}, [new RegExp("[a-z]+:\/\/"), new RegExp("^ftp:/")]);
// -> true
~~~

<h3 id="api-all"><code>all</code></h3>

Determine whether all the elements of a list satisfy some predicate.

~~~{.JavaScript}
Udon.all(function(str) {
    return str.match(/^[A-Z][a-z]+$/);
}, ["One", "Two", "three"]);
// -> false
~~~

<h3 id="api-none"><code>none</code></h3>

Check that no element of a list satisfies a predicate.

~~~{.JavaScript}
Udon.none(function(c) {
    return c === Math.PI;
}, [1, 0, -1, Math.LN2, Math.E]);
// -> true
~~~

<h3 id="api-sum"><code>sum</code></h3>

Adds the elements of a numeric list together.

~~~{.JavaScript}
Udon.sum([]);
// -> 0
Udon.sum([1, 2, 3, 4]);
// -> 10
~~~

<h3 id="api-product"><code>product</code></h3>

Multiplies the elements of a numeric list together.

~~~{.JavaScript}
Udon.product([]);
// -> 1
Udon.sum([1, 2, 3, 4]);
// -> 24
~~~

<h3 id="api-elem"><code>elem</code></h3>

Returns `true` if the given element is in the array, `false` otherwise.

~~~{.JavaScript}
Udon.elem(2, [1, 2, 3]) === true;

Udon.elem(4, [1, 2, 3]) === false;
~~~

<h3 id="api-notElem"><code>notElem</code></h3>

Returns `false` if the given element is in the array, and `true` if it is not.

~~~{.JavaScript}
Udon.notElem('Z', ['a', 'b', 'c']) === true;

Udon.notElem('Z', ['X', 'Y', 'Z']) === false;
~~~

<h3 id="api-partition"><code>partition</code></h3>

Separates a list into lists of those elements which do and do not satisfy some
predicate.

~~~{.JavaScript}
Udon.partition(function(n) {
    return n < 5;
}, [4,7,3,9,21,2]);
// -> [[4,3,2], [7,9,21]]
~~~

<h3 id="api-unfoldr"><code>unfoldr</code></h3>

Builds a list by repeatedly applying a function to a seed value. The function
should return a pair of values: the first is an element to append to the list
under construction, while the second is the seed value to pass to the next
function call. The function must return `null` when it's done, at which point
`unfoldr` will return the constructed list.

~~~{.JavaScript}
var randomInts = function(ceil, n) {
    return Udon.unfoldr(function(i) {
        return i < 1 ? null : [Math.floor(Math.random() * ceil), i - 1];
    }, n);
};

randomInts(5, 4); // -> [5,1,4,5] e.g.
~~~

<h3 id="api-zip"><code>zip</code></h3>

Transforms a pair of lists into a list of pairs. If the lists are of differing
lengths, any elements left over after each element in the shorter list has been
paired up with one from the longer list will be discarded.

~~~{.JavaScript}
Udon.zip([1,2,3], ["a", "b", "c"]); // -> [[1, "a"], [2, "b"], [3, "c"]]
~~~

<h3 id="api-zipwith"><code>zipWith</code></h3>

The `zipWith` function is a generalisation of `zip`: it returns the result of
applying a function to each pair of elements from two lists.

~~~{.JavaScript}
Udon.zipWith(function(a, b) {
    return a * b;
}, [1,2,3], [4,5,6]);
// -> [4,10,18]
~~~
