Udon = (typeof Udon === 'undefined') ? {} : Udon;

Udon._slice = Array.prototype.slice;

Udon.ncurry = function(n) {
    if (typeof n != 'number' || n < 2) n = 2;
    var curry = function(f) {
        var a1 = Udon._slice.call(arguments, 1);
        return function() {
            var a2 = a1.concat(Udon._slice.call(arguments, 0));
            if (a2.length < n) {
                a2.unshift(f);
                return curry.apply(null, a2);
            } else {
                return f.apply(null, a2);
            }
        };
    };
    return curry;
};

Udon.curry = function(f) {
    var ar = f.length, a1 = Udon._slice.call(arguments, 1),
    accumulator = function() {
        var a2 = a1.concat(Udon._slice.call(arguments, 0));
        return a2.length >= ar ?
            f.apply(null, a2) : Udon.curry.apply(null, [f].concat(a2));
    };
    return a1.length >= ar ? accumulator() : accumulator;
};

Udon.compose = function(fs, ar) {
    var composed = function() {
        var i  = fs.length - 1, x,
            as = Udon._slice.call(arguments, 0);
        if (i < 0) return as[0];
        x = fs[i].apply(null, as);
        while (i--) x = fs[i](x);
        return x;
    };
    return typeof ar == 'number' && ar > 1 ?
        Udon.ncurry(ar)(composed) : composed;
};

Udon.id = function(x) {
    return x;
};

Udon.foldl = function(f, z, xs) {
    var len = xs.length, i;
    for (i = 0; i < len; i++) z = f(z, xs[i]);
    return z;
};

Udon.foldl1 = function(f, xs) {
    var len = xs.length, z = xs[0], i;
    for (i = 1; i < len; i++) z = f(z, xs[i]);
    return z;
};

Udon.foldr = function(f, z, xs) {
    var i = xs.length;
    while (i--) z = f(xs[i], z);
    return z;
};

Udon.maximum = function(xs) {
    return Udon.foldl1(Math.max, xs);
};

Udon.minimum = function(xs) {
    return Udon.foldl1(Math.min, xs);
};

Udon.maximumBy = function(cmp, xs) {
    var maxBy = function(x, y) { return cmp(x, y) === 1 ? x : y; };
    return Udon.foldl1(maxBy, xs);
};

Udon.minimumBy = function(cmp, xs) {
    var minBy = function(x, y) { return cmp(x, y) === 1 ? y : x; };
    return Udon.foldl1(minBy, xs);
};

Udon.map = function(f, xs) {
    var i = xs.length, ys = new Array(i);
    while (i--) ys[i] = f(xs[i]);
    return ys;
};

Udon.filter = function(f, xs) {
    var ys = [], len = xs.length, i, e;
    for (i = 0; i < len; i++) {
        e = xs[i];
        if (f(e)) ys.push(e);
    }
    return ys;
};

Udon.any = function(p, xs) {
    var i = xs.length;
    while (i--) {
        if (p(xs[i])) return true;
    }
    return false;
};

Udon.all = function(p, xs) {
    var i = xs.length;
    while (i--) {
        if (!p(xs[i])) return false;
    }
    return true;
};

Udon.none = function(p, xs) {
    return !Udon.any(p, xs);
};

Udon.partition = function(f, xs) {
    var ts = [], fs = [], len = xs.length, i, e;
    for (i = 0; i < len; i++) {
        e = xs[i];
        (f(e) ? ts : fs).push(e);
    }
    return [ts, fs];
};

Udon.unfoldr = function(step, seed) {
    var output  = [], result;
    while ((result = step(seed))) {
        output.push(result[0]);
        seed = result[1];
    }
    return output;
};

Udon.zip = function(xs, ys) {
    return Udon.zipWith(function(x, y) { return [x, y]; }, xs, ys);
};

Udon.zipWith = function(f, xs, ys) {
    var xsl = xs.length, ysl = ys.length, i = xsl > ysl ? ysl : xsl,
        zs = new Array(i);
    while (i--) zs[i] = f(xs[i], ys[i]);
    return zs;
};
