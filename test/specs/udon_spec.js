JS.ENV.UdonSpec = JS.Test.describe('Udon', function() { with (this) {
    before(function() {
        this.Udon     = JS.ENV.Udon;
        this.add      = function(a, b) { return a + b; };
        this.multiply = function(a, b) { return a * b; };
        this.cons     = function(car, cdr) { return [car].concat(cdr); };
        this.rcons    = function(car, cdr) { return [cdr].concat(car); };
        this.pair     = function(x, y) { return [x, y]; };
        this.id       = function(x) { return x === x; };
        this.nid      = function(x) { return x !== x; };
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
        this.reverse  = function(ns) {
            return ns.concat([]).reverse();
        };
    });
    
    describe('curry', function() {
        it('makes new functions each time it is called', function() { with(this) {
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
        it('makes new functions of arity n', function() { with(this) {
            var add3   = function(a, b, c) { return a + b + c; },
                curry3 = Udon.ncurry(3),
                add3c  = curry3(add3);
            
            assertEqual(6, add3c(1, 2, 3));
            assertEqual(6, add3c(1, 2)(3));
            assertEqual(6, add3c(1)(2)(3));
        }});
        
        it('should not keep any state', function() { with(this) {
            var max3c = Udon.ncurry(3)(Math.max),
                max5  = max3c(5);
            
            assertEqual(10, max5(1)(10));
            assertEqual(5, max5(1, 2));
        }});
    });
    
    describe('compose', function() {
        it('should produce the same result as directly applying the composed functions', function() { with(this) {
            var tcs = Udon.compose([Math.sin, Math.cos, Math.tan]);
            
            assertEqual(Math.sin(Math.cos(Math.tan(0.7))), tcs(0.7));
        }});
        
        it('should accept an option arity argument making the composed function partially applicable', function() { with(this) {
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
    
    describe('foldl', function() {
        it('can be used to implement sum', function() { with(this) {
            var ys = [9, 7, 4, 18, 27, 91, 412];
            
            assertEqual(sum(ys), Udon.foldl(add, 0, ys));
            assertEqual(0, Udon.foldl(add, 0, []));
        }});
        
        it('can be used to implement product', function() { with(this) {
            var ys = [24, 17, 61, 12];
            
            assertEqual(product(ys), Udon.foldl(multiply, 1, ys));
            assertEqual(1, Udon.foldl(multiply, 1, []));
        }});
        
        it('is equivalent to reverse on arrays when passed rcons and []', function() { with(this) {
            var ys = ['foo', 'bar', 'baz'];
            
            assertEqual(reverse(ys), Udon.foldl(rcons, [], ys));
            assertEqual([], Udon.foldl(rcons, [], []));
        }});
    });
    
    describe('foldr', function() {
        it('is the identity on arrays when passed cons and []', function() { with (this) {
            var xs = [1, 2, 3, 4];
            
            assertEqual(xs, Udon.foldr(cons, [], xs));
        }});
        
        it('preserves list structure', function() { with(this) {
            var ys = [5, 10, 15, 20],
                ps = [5, [10, [15, [20, []]]]];
            
            assertEqual(ps, Udon.foldr(pair, [], ys));
        }});
        
        it('can be used to implement map', function() { with(this) {
            var ys    = [0.7, 5.3, 7.1, 3.9],
                fCons = function(f) {
                    return function(car, cdr) {
                        return [f(car)].concat(cdr);
                    };
                };
            
            assertEqual(Udon.map(Math.floor, ys),
                Udon.foldr(fCons(Math.floor), [], ys));
        }});
        
        it('can turn arrays into lists', function() { with(this) {
            var list = {car: 1, cdr: {car: 2, cdr: {car: null, cdr: null}}},
                cell = function(head, tail) { return {car: head, cdr: tail}; };
            
            assertEqual(list, Udon.foldr(cell, {car: null, cdr: null}, [1, 2]));
        }});
        
        it('can be used to implement maximum', function() { with(this) {
            var ys = [1, 2, 4, 9, 3, 7];
            
            assertEqual(9, Udon.foldr(Math.max, -Infinity, ys));
        }});
        
        it('can be used to implement minimum', function() { with(this) {
            var ys = [1, 2, 4, 9, 3, 7];
            
            assertEqual(1, Udon.foldr(Math.min, Infinity, ys));
        }});
    });
    
    describe('map', function() {
        it('should return an array of the same length as that given', function() { with(this) {
            var zs = [null, null, null, null];
            
            assertEqual(zs.length, Udon.map(id, zs).length);
        }});
        
        it('should return an empty array when given one', function() { with(this) {
            assertEqual([], Udon.map(id, []));
        }});
        
        it('should apply a function to each element of an array', function() { with(this) {
            assertEqual([1, 2, 3, 4], Udon.map(Math.floor, [1.7, 2.4, 3.9, 4.1]));
        }});
    });
    
    describe('filter', function() {
        it('is the identity on arrays when passed the identity function', function() { with (this) {
            var zs = [5, 4, 3, 2, 1];
            
            assertEqual(zs, Udon.filter(id, zs));
        }});
        
        it('returns the empty list when passed the negation of the identity function', function() { with (this) {
            var xs = [8, 4, 2, 0];
            
            assertEqual([], Udon.filter(nid, xs));
        }});
        
        it('only returns those elements to which a predicate applies', function() { with(this) {
            var zs         = [-1, 22, -17, 15],
                isNegative = function(n) { return n < 0; };
            
            assertEqual([-1, -17], Udon.filter(isNegative, zs));
        }});
    });
    
    describe('any', function() {
        it('should return true if one element of an array satisfies a predicate', function() { with(this) {
            var over5 = function(n) { return n > 5; };
            
            assert(Udon.any(over5, [7, 1, 2, -5, 0]));
        }});
        
        it('should return false if no elements of an array satisfy a predicate', function() { with(this) {
            var isOdd = function(n) { return n % 2 == 1; };
            
            assert(!Udon.any(isOdd, [0, 2, 4, 6, 8, 10, 12]));
        }});
        
        it('should return false if the supplied array is empty', function() { with(this) {
            assert(!Udon.any(id, []));
        }});
    });
    
    describe('all', function() {
        it('should return true if every element of an array satisfies a predicate', function() { with(this) {
            var isEven = function(n) { return n % 2 == 0; };
            
            assert(Udon.all(isEven, [2, 4, 8, 16]));
        }});
        
        it('should return false if any element of an array fails to satisfy a predicate', function() { with(this) {
            var isPositive = function(n) { return n > 0; };
            
            assert(!Udon.all(isPositive, [17, 9, 4, -2, 1, 0]));
        }});
        
        it('should return true if the supplied array is empty', function() { with(this) {
            assert(Udon.all(id, []));
        }});
    });
    
    describe('zip', function() {
        it('should create a list of pairs from a pair of lists', function() { with (this) {
            var z1 = Udon.zip([1, 2, 3], [1, 2, 3]),
                z2 = Udon.zip([1, 2], [1, 2, 3]),
                z3 = Udon.zip([1, 2, 3], [2, 3]);
            
            assertEqual([[1, 1], [2, 2], [3, 3]], z1);
            assertEqual([[1, 1], [2, 2]], z2);
            assertEqual([[1, 2], [2, 3]], z3);
        }});
        
        it('should return a list the same length as its shortest argument', function() { with (this) {
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
        it('should do pairwise addition of two lists', function() { with (this) {
            assertEqual([2, 6, 12],
                Udon.zipWith(add, [1, 2, 3], [1, 4, 9]));
        }});
        
        it('should produce the same output as `zip` when given a pairing function', function() { with (this) {
            var pair = function(a, b) { return [a, b]; };
            
            assertEqual(Udon.zip(['a', 'b', 'c'], [5, 10, 20]),
                Udon.zipWith(pair, ['a', 'b', 'c'], [5, 10, 20]));
        }});
    });
}});
