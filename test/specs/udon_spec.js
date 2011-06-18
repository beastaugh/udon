JS.ENV.UdonSpec = JS.Test.describe('Udon', function() { with (this) {
    before(function() {
        this.Udon     = JS.ENV.Udon;
        this.add      = function(a, b) { return a + b; };
        this.multiply = function(a, b) { return a * b; };
        this.cons     = function(car, cdr) { return [car].concat(cdr); };
        this.rcons    = function(car, cdr) { return [cdr].concat(car); };
        this.pair     = function(x, y) { return [x, y]; };
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
}});
