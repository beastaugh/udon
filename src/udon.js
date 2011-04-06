Udon = (typeof Udon === 'undefined') ? {} : Udon;

Udon._slice = Array.prototype.slice;

Udon.ncurry = function(n) {
    if (typeof n != 'number' || n < 2) n = 2;
    return function(f) {
        var a1 = Udon._slice.call(arguments, 1),
        accumulator = function() {
            var a2 = a1.concat(Udon._slice.call(arguments, 0));
            if (a2.length < n) {
                a2.unshift(f);
                a1 = a2;
                return accumulator;
            } else {
                return f.apply(null, a2);
            }
        };
        return accumulator;
    };
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

Udon.foldl = function(f, z, xs) {
    var len = xs.length, i;
    for (i = 0; i < len; i++) z = f(z, xs[i]);
    return z;
};

Udon.reduce = Udon.foldl;
Udon.inject = Udon.foldl;

Udon.foldr = function(f, z, xs) {
    var i = xs.length;
    while (i--) z = f(xs[i], z);
    return z;
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

Udon.select = Udon.filter;

Udon.any = function(p, xs) {
    var i = xs.length;
    while (i--) if (p(xs[i])) return true;
    return false;
};

Udon.all = function(p, xs) {
    var i = xs.length;
    while (i--) if (!p(xs[i])) return false;
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

Udon.each = function(f, xs) {
    var i = xs.length;
    while (i--) f(xs[i]);
};