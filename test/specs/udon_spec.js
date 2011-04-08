JS.ENV.UdonSpec = JS.Test.describe('Udon', function() { with (this) {
    before(function() {
        this.Udon = JS.ENV.Udon;
        this.add  = function(a, b) { return a + b; };
        this.cons = function(car, cdr) { return [car].concat(cdr); };
        this.pair = function(x, y) { return [x, y]; };
        this.id   = function(x) { return x === x; };
        this.nid  = function(x) { return x !== x; };
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
