JS.ENV.UdonSpec = JS.Test.describe('Udon', function() { with (this) {
    before(function() {
        this.Udon     = JS.ENV.Udon;
        this.add      = function(a, b) { return a + b; };
        this.multiply = function(a, b) { return a * b; };
        this.cons     = function(car, cdr) { return [car].concat(cdr); };
        this.rcons    = function(car, cdr) { return [cdr].concat(car); };
        this.pair     = function(x, y) { return [x, y]; };
        this.gt       = function(n) { return function(x) { return x > n; }; };
        this.lt       = function(n) { return function(x) { return x < n; }; };
        this.id       = function(x) { return true; };
        this.nid      = function(x) { return false; };
        this.sum      = function(ns) {
            var n = 0, m = ns.length;
            while (m--) n += ns[m];
            return n;
        };
        this.product  = function(ns) {
            var n = 1, m = ns.length;
            while (m--) n *= ns[m];
            return n;
        };
    });
    
    describe('curry', function() {
        it('`curry` makes new functions each time it is called', function() { with(this) {
            var plus10, minus5;
            
            plus10 = Udon.curry(add)(10);
            
            assertEqual(15, plus10(5));
            assertEqual(-5, plus10(-15));
            
            minus5 = Udon.curry(add)(-5);
            
            assertEqual(15, minus5(20));
            assertEqual(-5, minus5(0));
        }});
    });
    
    describe('ncurry', function() {
        it('`ncurry` makes new functions of arity n', function() { with(this) {
            var add3   = function(a, b, c) { return a + b + c; },
                curry3 = Udon.ncurry(3),
                add3c  = curry3(add3);
            
            assertEqual(6, add3c(1, 2, 3));
            assertEqual(6, add3c(1, 2)(3));
            assertEqual(6, add3c(1)(2)(3));
        }});
        
        it('`ncurry` should not keep any state', function() { with(this) {
            var max3c = Udon.ncurry(3)(Math.max),
                max5  = max3c(5);
            
            assertEqual(10, max5(1)(10));
            assertEqual(5, max5(1, 2));
        }});
    });
    
    describe('compose', function() {
        it('`compose` should produce the same result as directly applying the composed functions', function() { with(this) {
            var tcs = Udon.compose([Math.sin, Math.cos, Math.tan]);
            
            assertEqual(Math.sin(Math.cos(Math.tan(0.7))), tcs(0.7));
        }});
        
        it('`compose` should accept an option arity argument making the composed function partially applicable', function() { with(this) {
            var ceilMax  = Udon.compose([Math.ceil, Math.max], 2),
                floorMin = Udon.compose([Math.floor, Math.min], 3);
            
            assertEqual(2, ceilMax(0.7)(1.1));
            assertEqual(1, ceilMax(0.7)(0.4));
            assertEqual(3, ceilMax(1.5)(2.4));
            
            assertEqual(4, floorMin(4.5, 5.2, 7.4));
            assertEqual(6, floorMin(6.1, 9.8, 6.2));
            assertEqual(1, floorMin(1.3, 6.2, 4.9));
        }});
    });
    
    describe('id', function() {
        it('`id` returns the object given to it as an argument', function() { with(this) {
            assertEqual(1, Udon.id(1));
            assertEqual("foo", Udon.id("foo"));
            assertEqual({}, Udon.id({}));
            assertEqual([1, 2, 3], Udon.id([1, 2, 3]));
        }});
        
        it('`map` passed `id` should be `id` on arrays', function() { with(this) {
            assertEqual([1, 2, 3], Udon.map(Udon.id, [1, 2, 3]));
            assertEqual(Udon.id([1, 2, 3]), Udon.map(Udon.id, [1, 2, 3]));
        }});
        
        it('Any function composed with `id` should produce the same output as the uncomposed function', function() { with(this) {
            assertEqual(6, Udon.compose([Udon.id, sum])([1, 2, 3]));
            assertEqual(6, Udon.compose([sum, Udon.id])([1, 2, 3]));
        }});
    });
    
    describe('foldl', function() {
        it('`foldl` can be used to implement sum', function() { with(this) {
            var ys = [9, 7, 4, 18, 27, 91, 412];
            
            assertEqual(sum(ys), Udon.foldl(add, 0, ys));
            assertEqual(0, Udon.foldl(add, 0, []));
        }});
        
        it('`foldl` can be used to implement product', function() { with(this) {
            var ys = [24, 17, 61, 12];
            
            assertEqual(product(ys), Udon.foldl(multiply, 1, ys));
            assertEqual(1, Udon.foldl(multiply, 1, []));
        }});
        
        it('`foldl` is equivalent to reverse on arrays when passed rcons and []', function() { with(this) {
            var ys = ['foo', 'bar', 'baz'];
            
            assertEqual(Udon.reverse(ys), Udon.foldl(rcons, [], ys));
            assertEqual([], Udon.foldl(rcons, [], []));
        }});
    });
    
    describe('foldl1', function() {
        it('`foldl1` can be used to implement concatenation', function() { with(this) {
            var concat = function(a, b) {
                return a.toString() + b.toString();
            };
            
            assertEqual("194317", Udon.foldl1(concat, [19, 4, 317]));
        }});
    });
    
    describe('foldr', function() {
        it('`foldr` is the identity on arrays when passed cons and []', function() { with (this) {
            var xs = [1, 2, 3, 4];
            
            assertEqual(xs, Udon.foldr(cons, [], xs));
        }});
        
        it('`foldr` preserves list structure', function() { with(this) {
            var ys = [5, 10, 15, 20],
                ps = [5, [10, [15, [20, []]]]];
            
            assertEqual(ps, Udon.foldr(pair, [], ys));
        }});
        
        it('`foldr` can be used to implement map', function() { with(this) {
            var ys    = [0.7, 5.3, 7.1, 3.9],
                fCons = function(f) {
                    return function(car, cdr) {
                        return [f(car)].concat(cdr);
                    };
                };
            
            assertEqual(Udon.map(Math.floor, ys),
                Udon.foldr(fCons(Math.floor), [], ys));
        }});
        
        it('`foldr` can turn arrays into lists', function() { with(this) {
            var list = {car: 1, cdr: {car: 2, cdr: {car: null, cdr: null}}},
                cell = function(head, tail) { return {car: head, cdr: tail}; };
            
            assertEqual(list, Udon.foldr(cell, {car: null, cdr: null}, [1, 2]));
        }});
        
        it('`foldr` can be used to implement maximum', function() { with(this) {
            var ys = [1, 2, 4, 9, 3, 7];
            
            assertEqual(9, Udon.foldr(Math.max, -Infinity, ys));
        }});
        
        it('`foldr` can be used to implement minimum', function() { with(this) {
            var ys = [1, 2, 4, 9, 3, 7];
            
            assertEqual(1, Udon.foldr(Math.min, Infinity, ys));
        }});
    });
    
    describe('foldr1', function() {
        it('`foldr1` is equivalent to foldr when passed add and 0', function() { with(this) {
            var xs = [1, 2, 4, 9, 3, 7];
            
            assertEqual(Udon.foldr(add, 0, xs), Udon.foldr1(add, xs));
        }});
        it('`foldr1` is equivalent to foldr when passed multiply and 1', function() { with(this) {
            var xs = [1, 2, 4, 9, 3, 7];
            
            assertEqual(Udon.foldr(multiply, 1, xs), Udon.foldr1(multiply, xs));
        }});
    });
                                                                       
    
    describe('concat', function() {
        it('`concat` flattens an array one level', function() { with(this) {
            var xs = [1, 2, 3, 4, 5, 6, 7, 8],
                ys = [[1, 2], [3, 4], [5, 6], [7, 8]],
                zs = [[[1, 2], [3, 4]], [[5, 6], [7, 8]]];
            
            assertEqual(xs, Udon.concat(ys));
            assertEqual(ys, Udon.concat(zs));
        }});
    });
    
    describe('maximum', function() {
        it('`maximum` should return the largest number in an array', function() { with(this) {
            var ns = [4, 1, 7, 12, -4, 0, 8];
            
            assertEqual(12, Udon.maximum(ns));
        }});
    });
    
    describe('minimum', function() {
        it('`minimum` should return the smallest number in an array', function() { with(this) {
            var ns = [4, 1, 7, 12, -4, 0, 8];
            
            assertEqual(-4, Udon.minimum(ns));
        }});
    });
    
    describe('maximumBy', function() {
        it('`maximumBy` should return the maximum of an array, given some user-defined comparison', function() { with(this) {
            var nearerZero = function(x, y) {
                var ax = Math.abs(x),
                    ay = Math.abs(y);
                
                if (ax === ay) {
                    return 0;
                } else if (ax > ay) {
                    return -1;
                } else {
                    return 1;
                }
            };
            
            assertEqual(-2, Udon.maximumBy(nearerZero, [12, 15, -5, 7, 4, -2]));
        }});
    });
    
    describe('minimumBy', function() {
        it('`minimumBy` should return the minimum of an array, given some user-defined comparison', function() { with(this) {
            var longerThan = function(a, b) {
                var lenA = a.length, lenB = b.length;
                
                if (lenA === lenB) {
                    return 0;
                } else if (lenA > lenB) {
                    return 1;
                } else {
                    return -1;
                }
            };
            
            assertEqual("foo", Udon.minimumBy(longerThan, ["foo", "foobar", "foobarbaz"]));
        }});
    });
    
    describe('map', function() {
        it('`map` should return an array of the same length as that given', function() { with(this) {
            var zs = [null, null, null, null];
            
            assertEqual(zs.length, Udon.map(id, zs).length);
        }});
        
        it('`map` should return an empty array when given one', function() { with(this) {
            assertEqual([], Udon.map(id, []));
        }});
        
        it('`map` should apply a function to each element of an array', function() { with(this) {
            assertEqual([1, 2, 3, 4], Udon.map(Math.floor, [1.7, 2.4, 3.9, 4.1]));
        }});
    });
    
    describe('concatMap', function() {
        it('`concatMap` should return an array of the same length as that given', function() { with(this) {
            var xs = [[1], [2], [3], [4]];
            
            assertEqual(xs.length, Udon.concatMap(function(x){return x}, xs).length);
        }});
        it('`concatMap` function(x) { return cons(x, [])} should xs should return xs', function() { with(this) {
            var xs = [1, 2, 3, 4];
            
            assertEqual(xs, Udon.concatMap(function(x){return cons(x, [])}, xs));
        }});
    });
    
    describe('reverse', function() {
        it('`reverse` should return an array of the same length as that given', function() { with(this) {
            var zs = [null, null, null, null];
            
            assertEqual(zs.length, Udon.reverse(zs).length);
        }});
        
        it('`reverse` should produce an equivalent array to the reverse method', function() { with(this) {
            var xs = [1, 4, 9, 16],
                ys = Udon.reverse(xs);
            
            assertEqual(xs.reverse(), ys);
        }});
        
        it('`reverse` should return an empty array when given one', function() { with(this) {
            assertEqual([], Udon.reverse([]));
        }});
    });
    
    describe('intersperse', function() {
        it('`intersperse` should return the input array if it is empty or a singleton', function() { with(this) {
            assertEqual([], Udon.intersperse('a', []));
            assertEqual(['b'], Udon.intersperse('a', ['b']));
        }});
        
        it('`intersperse` should produce the same result as join with that separator when joined with the empty string', function() { with(this) {
            var xs = ['foo', 'bar', 'baz'];
            
            assertEqual(xs.join(':'), Udon.intersperse(':', xs).join(''));
        }});
        
        it('`intersperse` should intersperse the same separator between all its elements', function() { with(this) {
            var xs = [2, 4, 6, 8, 10];
            
            assertEqual([2, 1, 4, 1, 6, 1, 8, 1, 10], Udon.intersperse(1, xs));
        }});
    });
    
    describe('intercalate', function() {
        it('`intercalate` should intersperse its first arguments between ', function() { with(this) {
            var xs  = [1, 2, 3],
                xss = [[10, 20], [30, 40], [50, 60]];
            
            assertEqual([10, 20, 1, 2, 3, 30, 40, 1, 2, 3, 50, 60], Udon.intercalate(xs, xss));
        }});
    });
    
    describe('filter', function() {
        it('`filter` is the identity on arrays when passed the identity function', function() { with (this) {
            var zs = [5, 4, 3, 2, 1];
            
            assertEqual(zs, Udon.filter(id, zs));
        }});
        
        it('`filter` returns the empty list when passed the negation of the identity function', function() { with (this) {
            var xs = [8, 4, 2, 0];
            
            assertEqual([], Udon.filter(nid, xs));
        }});
        
        it('`filter` only returns those elements to which a predicate applies', function() { with(this) {
            var zs         = [-1, 22, -17, 15],
                isNegative = function(n) { return n < 0; };
            
            assertEqual([-1, -17], Udon.filter(isNegative, zs));
        }});
    });
    
    describe('any', function() {
        it('`any` should return true if one element of an array satisfies a predicate', function() { with(this) {
            var matchesHttp = function(regex) { return regex.exec("http://"); };
            
            assert(Udon.any(matchesHttp, [new RegExp("[a-z]+:\/\/"), new RegExp("^ftp:/")]));
        }});
        
        it('`any` should return false if no elements of an array satisfy a predicate', function() { with(this) {
            var isOdd = function(n) { return n % 2 == 1; };
            
            assert(!Udon.any(isOdd, [0, 2, 4, 6, 8, 10, 12]));
        }});
        
        it('`any` should return false if the supplied array is empty', function() { with(this) {
            assert(!Udon.any(id, []));
        }});
    });
    
    describe('all', function() {
        it('`all` should return true if every element of an array satisfies a predicate', function() { with(this) {
            var isEven = function(n) { return n % 2 == 0; };
            
            assert(Udon.all(isEven, [2, 4, 8, 16]));
        }});
        
        it('`all` should return false if any element of an array fails to satisfy a predicate', function() { with(this) {
            var isPositive = function(n) { return n > 0; };
            
            assert(!Udon.all(isPositive, [17, 9, 4, -2, 1, 0]));
        }});
        
        it('`all` should return true if the supplied array is empty', function() { with(this) {
            assert(Udon.all(id, []));
        }});
    });
    
    describe('none', function() {
        it('`none` should return true if no element of an array satisfies a predicate', function() { with(this) {
            var isUppercase = function(str) { return str.toUpperCase() == str; };
            
            assert(Udon.none(isUppercase, ["foo", "Bar", "BAz"]));
        }});
        
        it('`none` should return true if the supplied array is empty', function() { with(this) {
            assert(Udon.none(id, []));
        }});
    });
    
    describe('sum', function() {
        it('`sum` should return 0 when given an empty array', function() { with(this) {
            assertEqual(0, Udon.sum([]));
        }});
        
        it('`sum` should add the elements of a numeric array', function() { with(this) {
            assertEqual(10, Udon.sum([1, 2, 3, 4]));
        }});
    });
    
    describe('product', function() {
        it('`sum` should return 1 when given an empty array', function() { with(this) {
            assertEqual(1, Udon.product([]));
        }});
        
        it('`sum` should multiply the elements of a numeric array', function() { with(this) {
            assertEqual(24, Udon.product([1, 2, 3, 4]));
        }});
    });
    
    describe('elem', function() {
        it('`elem` should return false when an element is not present in an array', function() { with(this) {
            assertEqual(false, Udon.elem(5, []));
            assertEqual(false, Udon.elem(5, [1, 2, 3, 4]));
        }});
        
        it('`elem` should return true when an element is present in an array', function() { with(this) {
            assert(Udon.elem(5, [1, 2, 3, 4, 5]));
            assert(Udon.elem(5, [5, 4, 3, 2, 1]));
        }});
    });
    
    describe('notElem', function() {
        it('`notElem` should return true when an element is not present in an array', function() { with(this) {
            assert(Udon.notElem(5, []));
            assert(Udon.notElem(5, [1, 2, 3, 4]));
        }});
        
        it('`notElem` should return false when an element is present in an array', function() { with(this) {
            assertEqual(false, Udon.notElem(5, [1, 2, 3, 4, 5]));
            assertEqual(false, Udon.notElem(5, [5, 4, 3, 2, 1]));
        }});
    });
    
    describe('partition', function() {
        it('`partition` should partition the elements of an array into arrays of those satisfying and failing to satisfy some predicate', function() { with(this) {
            var isNegative = function(n) { return n < 0; };
            
            assertEqual([[-1, -4], [0, 5, 2]],
                Udon.partition(isNegative, [0, -1, 5, 2, -4]));
        }});
        
        it('`partition` should return an empty first array if no element satisfies the given predicate', function() { with(this) { 
            var isNull = function(x) { return x === null; };
            
            assertEqual([[], ["foo", "bar"]],
                Udon.partition(isNull, ["foo", "bar"]));
        }});
        
        it('`partition` should return an empty second array if every element satisfies the given predicate', function() { with(this) {
            var isString = function(x) { return typeof x == 'string'; };
            
            assertEqual([["aaa", "bbb", "ccc"], []],
                Udon.partition(isString, ["aaa", "bbb", "ccc"]));
        }});
        
        it('`partition` should return a pair of empty arrays if supplied with an empty array', function() { with(this) {
            assertEqual([[], []], Udon.partition(id, []));
        }});
    });
    
    describe('zip', function() {
        it('`zip` should create a list of pairs from a pair of lists', function() { with (this) {
            var z1 = Udon.zip([1, 2, 3], [1, 2, 3]),
                z2 = Udon.zip([1, 2], [1, 2, 3]),
                z3 = Udon.zip([1, 2, 3], [2, 3]);
            
            assertEqual([[1, 1], [2, 2], [3, 3]], z1);
            assertEqual([[1, 1], [2, 2]], z2);
            assertEqual([[1, 2], [2, 3]], z3);
        }});
        
        it('`zip` should return a list the same length as its shortest argument', function() { with (this) {
            var z1 = Udon.zip([], [1, 2, 3]),
                z2 = Udon.zip([1, 2, 3], []),
                z3 = Udon.zip([1, 2], [3, 4]),
                z4 = Udon.zip([1], [2, 3]);
            
            assertEqual(0, z1.length);
            assertEqual(0, z2.length);
            assertEqual(2, z3.length);
            assertEqual(1, z4.length);
        }});
    });
    
    describe('zipWith', function() {
        it('`zipWith` should do pairwise addition of two lists', function() { with (this) {
            assertEqual([2, 6, 12],
                Udon.zipWith(add, [1, 2, 3], [1, 4, 9]));
        }});
        
        it('`zipWith` should produce the same output as `zip` when given a pairing function', function() { with (this) {
            var pair = function(a, b) { return [a, b]; };
            
            assertEqual(Udon.zip(['a', 'b', 'c'], [5, 10, 20]),
                Udon.zipWith(pair, ['a', 'b', 'c'], [5, 10, 20]));
        }});
    });
    
    describe('head', function() {
        it('`head` should return the first element from an array', function() { with (this) {
            assertEqual(1, Udon.head([1, 2, 3, 4]));
        }});
    });
    
    describe('init', function() {
        it('`init` should return everything from an array but the last element', function() { with (this) {
            assertEqual([1, 2, 3], Udon.init([1, 2, 3, 4]));
        }});
        
        it('`init` should return return an empty array given a one-element array', function() { with (this) {
            assertEqual([], Udon.init([4]));
        }});
    });
    
    describe('tail', function() {
        it('`tail` should return everything from an array but the first element', function() { with (this) {
            assertEqual([2, 3, 4], Udon.tail([1, 2, 3, 4]));
        }});
        
        it('`tail` should return return an empty array given a one-element array', function() { with (this) {
            assertEqual([], Udon.tail([4]));
        }});
    });
    
    describe('last', function() {
        it('`last` should return the last element from an array', function() { with (this) {
            assertEqual(4, Udon.last([1, 2, 3, 4]));
            assertEqual(4, Udon.last([4]));
        }});
    });
    
    describe('and', function() {
        it('`and` should return the true if and only if every element of an array is true', function() { with (this) {
            assertEqual(true, Udon.and([true, true, true]));
            assertEqual(false, Udon.and([true, true, false]));
        }});
    });
    
    describe('or', function() {
        it('`or` should return the true if any element of an array is true', function() { with (this) {
            assertEqual(true, Udon.or([true, true, true]));
            assertEqual(true, Udon.or([false, false, true]));
            assertEqual(false, Udon.or([false, false, false]));
        }});
    });
    
    describe('append', function() {
        it('`append` should combine two arrays', function() { with (this) {
            assertEqual([1, 2, 3, 4, 5, 6], Udon.append([1, 2, 3], [4, 5, 6]));
        }});
    });
    
    describe('empty', function() {
        it('`empty` should return true when passed an empty object', function() { with (this) {
            assertEqual(true, Udon.empty([]));
            assertEqual(true, Udon.empty({}));
            assertEqual(false, Udon.empty([1]));
        }});
    });
    
    describe('transpose', function() {
        it('`transpose` should transpose the rows and columns of an array', function() { with (this) {
            assertEqual([[1, 4], [2, 5], [3, 6]],
                        Udon.transpose([[1, 2, 3], [4, 5, 6]]));
            assertEqual([[1, 4, 7], [5, 8], [9]],
                        Udon.transpose([[1], [4, 5], [7, 8, 9]]));
            assertEqual([[1, 10], [2, 20], [3, 30], [4, 40]],
                        Udon.transpose([[1, 2, 3, 4], [10, 20, 30, 40]]));
        }});
    });
    
    describe('subsequences', function() {
        it('`subsequences` should return an array of all the subsequences of a given array', function() { with (this) {
            var a = [1, 2],
                b = [1, 2, 3];
            
            assertEqual([[], [1], [2], [1, 2]],
                        Udon.subsequences(a));
            assertEqual([[], [1], [2], [1, 2], [3], [1, 3], [2, 3], [1, 2, 3]],
                        Udon.subsequences(b));
            assertEqual(a, [1, 2]);
            assertEqual(b, [1, 2, 3]);
        }});
    });
    
    describe('permutations', function() {
        it('`permutations` should return an array of all the permutations of a given array', function() { with (this) {
            var origs = [[], [1], [1, 2], [1, 2, 3], [1, 2, 3, 4],
                         [1, 2, 3, 4, 5], [1, 2, 3, 4, 5, 6], [1, 2, 3, 4, 5, 6, 7], [1, 2, 3, 4, 5, 6, 7, 8]],
                perms = Udon.map(Udon.permutations, origs),
                facts = [0, 1, 2, 6, 24, 120, 720, 5040, 40320, 362880],
                i;
            
            assertEqual(origs.length, perms.length);
            
            /* the permutations should have n! elements each, with the special
               condition that permutations([]) returns the empty list, which in
               javascript is length 0 */
            
            for (i = 0; i < origs.length; i++) {
                assertEqual(facts[i], perms[i].length);
            }
            
            // some calculated permutations from GHC
            assertEqual(true, Udon.empty(perms[0]));
            assertEqual([[1]], perms[1]);
            assertEqual([[1, 2], [2, 1]].sort(), perms[2].sort());
            assertEqual([[1, 2, 3], [1, 3, 2], [2, 1, 3], [2, 3, 1], [3, 1, 2], [3, 2, 1]].sort(),
                        perms[3].sort());
            assertEqual([[1, 2, 3, 4], [2, 1, 3, 4], [3, 2, 1, 4], [2, 3, 1, 4], [3, 1, 2, 4],
                         [1, 3, 2, 4], [4, 3, 2, 1], [3, 4, 2, 1], [3, 2, 4, 1], [4, 2, 3, 1],
                         [2, 4, 3, 1], [2, 3, 4, 1], [4, 1, 2, 3], [1, 4, 2, 3], [1, 2, 4, 3],
                         [4, 2, 1, 3], [2, 4, 1, 3], [2, 1, 4, 3], [4, 1, 3, 2], [1, 4, 3, 2],
                         [1, 3, 4, 2], [4, 3, 1, 2], [3, 4, 1, 2], [3, 1, 4, 2]].sort(),
                        perms[4].sort());
            assertEqual([[1, 2, 3, 4, 5], [2, 1, 3, 4, 5], [3, 2, 1, 4, 5], [2, 3, 1, 4, 5], [3, 1, 2, 4, 5],
                         [1, 3, 2, 4, 5], [4, 3, 2, 1, 5], [3, 4, 2, 1, 5], [3, 2, 4, 1, 5], [4, 2, 3, 1, 5],
                         [2, 4, 3, 1, 5], [2, 3, 4, 1, 5], [4, 1, 2, 3, 5], [1, 4, 2, 3, 5], [1, 2, 4, 3, 5],
                         [4, 2, 1, 3, 5], [2, 4, 1, 3, 5], [2, 1, 4, 3, 5], [4, 1, 3, 2, 5], [1, 4, 3, 2, 5],
                         [1, 3, 4, 2, 5], [4, 3, 1, 2, 5], [3, 4, 1, 2, 5], [3, 1, 4, 2, 5], [5, 4, 3, 2, 1],
                         [4, 5, 3, 2, 1], [4, 3, 5, 2, 1], [4, 3, 2, 5, 1], [5, 3, 4, 2, 1], [3, 5, 4, 2, 1],
                         [3, 4, 5, 2, 1], [3, 4, 2, 5, 1], [5, 2, 3, 4, 1], [2, 5, 3, 4, 1], [2, 3, 5, 4, 1],
                         [2, 3, 4, 5, 1], [5, 3, 2, 4, 1], [3, 5, 2, 4, 1], [3, 2, 5, 4, 1], [3, 2, 4, 5, 1],
                         [5, 2, 4, 3, 1], [2, 5, 4, 3, 1], [2, 4, 5, 3, 1], [2, 4, 3, 5, 1], [5, 4, 2, 3, 1],
                         [4, 5, 2, 3, 1], [4, 2, 5, 3, 1], [4, 2, 3, 5, 1], [5, 1, 2, 3, 4], [1, 5, 2, 3, 4],
                         [1, 2, 5, 3, 4], [1, 2, 3, 5, 4], [5, 2, 1, 3, 4], [2, 5, 1, 3, 4], [2, 1, 5, 3, 4],
                         [2, 1, 3, 5, 4], [5, 2, 3, 1, 4], [2, 5, 3, 1, 4], [2, 3, 5, 1, 4], [2, 3, 1, 5, 4],
                         [5, 1, 3, 2, 4], [1, 5, 3, 2, 4], [1, 3, 5, 2, 4], [1, 3, 2, 5, 4], [5, 3, 1, 2, 4],
                         [3, 5, 1, 2, 4], [3, 1, 5, 2, 4], [3, 1, 2, 5, 4], [5, 3, 2, 1, 4], [3, 5, 2, 1, 4],
                         [3, 2, 5, 1, 4], [3, 2, 1, 5, 4], [5, 1, 4, 3, 2], [1, 5, 4, 3, 2], [1, 4, 5, 3, 2],
                         [1, 4, 3, 5, 2], [5, 4, 1, 3, 2], [4, 5, 1, 3, 2], [4, 1, 5, 3, 2], [4, 1, 3, 5, 2],
                         [5, 4, 3, 1, 2], [4, 5, 3, 1, 2], [4, 3, 5, 1, 2], [4, 3, 1, 5, 2], [5, 1, 3, 4, 2],
                         [1, 5, 3, 4, 2], [1, 3, 5, 4, 2], [1, 3, 4, 5, 2], [5, 3, 1, 4, 2], [3, 5, 1, 4, 2],
                         [3, 1, 5, 4, 2], [3, 1, 4, 5, 2], [5, 3, 4, 1, 2], [3, 5, 4, 1, 2], [3, 4, 5, 1, 2],
                         [3, 4, 1, 5, 2], [5, 1, 4, 2, 3], [1, 5, 4, 2, 3], [1, 4, 5, 2, 3], [1, 4, 2, 5, 3],
                         [5, 4, 1, 2, 3], [4, 5, 1, 2, 3], [4, 1, 5, 2, 3], [4, 1, 2, 5, 3], [5, 4, 2, 1, 3],
                         [4, 5, 2, 1, 3], [4, 2, 5, 1, 3], [4, 2, 1, 5, 3], [5, 1, 2, 4, 3], [1, 5, 2, 4, 3],
                         [1, 2, 5, 4, 3], [1, 2, 4, 5, 3], [5, 2, 1, 4, 3], [2, 5, 1, 4, 3], [2, 1, 5, 4, 3],
                         [2, 1, 4, 5, 3], [5, 2, 4, 1, 3], [2, 5, 4, 1, 3], [2, 4, 5, 1, 3], [2, 4, 1, 5, 3]].sort(),
                        perms[5].sort());
            
            // make sure lists with duplicates are handled correctly
            assertEqual(Udon.permutations([1, 2, 2]),
                        [[1, 2, 2], [1, 2, 2], [2, 1, 2], [2, 2, 1], [2, 1, 2], [2, 2, 1]]);
        }});
    });
    
    describe('cons', function() {
        it('`cons` should add an element to the front of the list', function() { with (this) {
            assertEqual([1], Udon.cons(1, []));
            assertEqual([[1], 2, 3], Udon.cons([1], [2, 3]));
        }});
    });
    
    describe('equal', function() {
        it('`equal` should determine value equality', function() { with (this) {
            assertEqual(true, Udon.equal(1, 1));
            assertEqual(false, Udon.equal(1, 2));
            assertEqual(true, Udon.equal("hello", "hello"));
            assertEqual(false, Udon.equal("hello", "Hello"));
            assertEqual(true, Udon.equal([1, 2, 3], [1, 2, 3]));
            assertEqual(true, Udon.equal([[1], [2], [3]], [[1], [2], [3]]));
            assertEqual(false, Udon.equal([[1], [2], [3]], [[1], [2, 3]]));
            assertEqual(false, Udon.equal([1], 1));
        }});
    });
    
    describe('scanl', function() {
        it('`scanl` should produce a list of continuous folds', function() { with (this) {
            assertEqual([0, 1, 3, 6], Udon.scanl(add, 0, [1, 2, 3]));
        }});
        
        it ('`the last element given by scanl should be the result of foldl', function() { with (this) {
            assertEqual(Udon.last(Udon.scanl(add, 10, [6, 99, 437])),
                        Udon.foldl(add, 10, [6, 99, 437]));
        }});
    });
    
    describe('scanl1', function() {
        it('`scanl1` should produce a list of continuous folds without a starting value', function() { with (this) {
            var a = [1, 2, 3];
            
            assertEqual([1, 3, 6], Udon.scanl1(add, a));
            assertEqual(a, [1, 2, 3]);
        }});
        
        it('`scanl1` can produce factorials from 1 with multiplication', function() { with (this) {
            var a     = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20],
                facts = [1, 2, 6, 24, 120, 720, 5040, 40320, 362880, 3628800,
                         39916800, 479001600, 6227020800, 87178291200,
                         1307674368000, 20922789888000, 355687428096000,
                         6402373705728000, 121645100408832000, 2432902008176640000];
            
            assertEqual(facts, Udon.scanl1(multiply, a));
        }});
    });
    
    describe('scanr', function() {
        it('`scanr` should produce a list of continuous folds from the right', function() { with (this) {
            assertEqual([6, 5, 3, 0], Udon.scanr(add, 0, [1, 2, 3]));
        }});
        
        it ('`the first element given by scanr should be the result of foldr', function() { with (this) {
            assertEqual(Udon.head(Udon.scanr(add, 10, [6, 99, 437])),
                        Udon.foldr(add, 10, [6, 99, 437]));
        }});
    });
    
    describe('scanr1', function() {
        it('`scanr1` should produce a list of continuous folds from the right without a starting value', function() { with (this) {
            var a = [1, 2, 3];
            
            assertEqual([6, 5, 3], Udon.scanr1(add, a));
            assertEqual(a, [1, 2, 3]);
        }});
    });
    
    describe('dropWhile', function() {
        it('`dropWhile` should remove the first N elements from a list that satisfy a given pred', function() { with (this) {
            var a = [1, 2, 3, 4, 5, 6];
            
            assertEqual([4, 5, 6], Udon.dropWhile(function(x){return x<=3}, a));
            assertEqual(a, [1, 2, 3, 4, 5, 6]);
        }});
        
        it('`dropWhile` should return the entire list when the first element of the supplied list does not satisfy the given predicate', function() { with (this) {
            var a = [1, 3, 5];
            
            assertEqual([1, 3, 5], Udon.dropWhile(function(x){return x % 2 === true}, a));
        }});
    });
    
    describe('take', function() {
        it('`take` should return the first N elements from a list', function() { with (this) {
            var a = [1, 2, 3],
                b = [1];
            
            assertEqual([1], Udon.take(1, a));
            assertEqual([1, 2], Udon.take(2, a));
            assertEqual([1], Udon.take(1, b));
            assertEqual(a, [1, 2, 3]);
        }});
        
        it('`take` should return the entire list when given an N greater than the length of the supplied list', function() { with (this) {
            var a = [1, 2, 3];
            
            assertEqual([1, 2, 3], Udon.take(4, a));
            assertEqual(a, [1, 2, 3]);
        }});
        
        it('`take` should return the empty list when asked for 0 elements', function() { with (this) {
            var a = [1, 2, 3];
            
            assertEqual([], Udon.take(0, a));
            assertEqual(a, [1, 2, 3]);
        }});
    });

    describe('takeWhile', function() {
        it('`takeWhile` should return the first N elements from a list that satisfy a supplied pred', function() { with (this) {
            var a = [1, 2, 3, 4, 5, 6],
                b = [2, 4, 3, 6];
            
            assertEqual([1, 2, 3], Udon.takeWhile(function(x){return x <= 3}, a));
            assertEqual([2, 4], Udon.takeWhile(function(x){return (x % 2) === 0}, b));
            assertEqual(a, [1, 2, 3, 4, 5, 6]);
        }});
        
        it('`takeWhile` should return the empty list if the first element of the given list does not satisfy supplied pred', function() { with (this) {
            var a = [1, 2, 3, 4, 5, 6];
            
            assertEqual([], Udon.takeWhile(function(x){return x >= 3}, a));
        }});
    });
    
    describe('splitAt', function() {
        it('`splitAt` should return a list containing a list of the first N elements from a list, and a list containing the remaining elements', function() { with (this) {
            var a = [1, 2, 3, 4, 5, 6];
            
            assertEqual([[1, 2, 3], [4, 5, 6]], Udon.splitAt(3, a));
            assertEqual(a, [1, 2, 3, 4, 5, 6]);
        }});
    });
    
    describe('span', function() {
        it('`span` should be equivalent to [takeWhile..., dropWhile...]', function() { with (this) {
            var a = [1, 2, 3, 4, 1, 2, 3, 4],
                b = [1, 2, 3];
            
            assertEqual([[1, 2], [3, 4, 1, 2, 3, 4]], Udon.span(lt(3), a));
            assertEqual([[1, 2], [3]], Udon.span(lt(3), b));
            assertEqual([[1, 2, 3], []], Udon.span(lt(9), b));
            assertEqual([[], [1, 2, 3]], Udon.span(lt(0), b));
        }});
    });

    describe('break', function() {
        it('`break` should be equivalent to span (not . p)', function() { with (this) {
            var a  = [1, 2, 3, 4, 1, 2, 3, 4],
                b  = [1, 2, 3];
            
            assertEqual([[], [1, 2, 3, 4, 1, 2, 3, 4]], Udon.break(lt(3), a));
            assertEqual([[1, 2, 3], [4, 1, 2, 3, 4]], Udon.break(gt(3), a));
        }});
    });
    
    describe('inits', function() {
        it('`inits` should return a list of lists containing all inits from a supplied list', function() { with (this) {
            var a = [1, 2, 3],
                b = [1];
            
            assertEqual([[], [1], [1, 2], [1, 2, 3]], Udon.inits(a));
            assertEqual([[], [1]], Udon.inits(b));
            assertEqual([1, 2, 3], a);
            assertEqual([1], b);
        }});
    });
    
    describe('tails', function() {
        it('`tails` should return a list of lists containing all tails from a supplied list', function() { with (this) {
            var a = [1, 2, 3];
            
            assertEqual([[1, 2, 3], [2, 3], [3], []], Udon.tails(a));
            assertEqual([1, 2, 3], a);
        }});
    });
    
    describe('find', function() {
        it('`find` should return the first element of a list that satisfies a given predicate', function() { with (this) {
            var a = [1, 3, 5, 6, 9, 10];
            
            assertEqual(6, Udon.find(function(x){return x % 2 === 0}, a));
            assertEqual([1, 3, 5, 6, 9, 10], a);
        }});
        
        it('`find` should return null if no element in a list satisfies a given predicate', function() { with (this) {
            var a = [1, 3, 5, 6, 9, 10];
            
            assertEqual(null, Udon.find(gt(11), a));
        }});
    });
    
    describe('elemIndex', function() {
        it('`elemIndex` should return the index of the first element in a list that is equal to a supplied element', function() { with (this) {
            var a = [1, 3, 5, 6, 9, 10],
                b = [[1, 2, 3], [4, 5, 6]];
                
            assertEqual(3, Udon.elemIndex(6, a));
            assertEqual(1, Udon.elemIndex([4, 5, 6], b));
            assertEqual([1, 3, 5, 6, 9, 10], a);
        }});
        it('`elemIndex` should return null if there is not element in a list equal to a supplied element', function() { with (this) {
            var a = [1, 3, 5, 6, 9, 10];
            
            assertEqual(null, Udon.elemIndex(11, a));
            assertEqual([1, 3, 5, 6, 9, 10], a);
        }});
    });
    
    describe('findIndex', function() {
        it('`findIndex` should return the index of the first element in a list that satisfies a given predicate', function() { with (this) {
            var a = [1, 3, 5, 6, 9, 10];
            
            assertEqual(4, Udon.findIndex(gt(6), a));
            assertEqual([1, 3, 5, 6, 9, 10], a);
        }});
        
        it('`findIndex` should return null if there is not element in a list that satisfies a given predicate', function() { with (this) {
            var a = [1, 3, 5, 6, 9, 10];
            
            assertEqual(null, Udon.findIndex(gt(11), a));
            assertEqual([1, 3, 5, 6, 9, 10], a);
        }});
    });
}});
