Udon
====

Functional programming has a number of common, idiomatic operations: maps,
folds, currying and so on. There are a number of excellent JavaScript libraries
which provide access to these fundamentals in a more-or-less functional style.
[Ojay][ojay]'s core extensions, Oliver Steele's [Functional][functionaljs] and
Jeremy Ashkenas' [Underscore][underscore] are all good examples.

Udon is cast in a similar mould to the above. It aims to provide basic support
for functional programming in JavaScript, initially by porting some elements of
Haskell's `Data.List` library.


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
        Udon.foldr(function(head, tail) {
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

    zip([1,2,3], ["a", "b", "c"]); // -> [[1, "a"], [2, "b"], [3, "c"]]

### `zipWith`

The `zipWith` function is a generalisation of `zip`: it returns the result of
applying a function to each pair of elements from two lists.

    zipWith(function(a, b) {
        return a * b;
    }, [1,2,3], [4,5,6]);
    // -> [4,10,18]


[ojay]:         http://ojay.othermedia.org/
[underscore]:   http://documentcloud.github.com/underscore/
[functionaljs]: http://osteele.com/sources/javascript/functional/
[fold]:         http://en.wikipedia.org/wiki/Fold_(higher-order_function)
