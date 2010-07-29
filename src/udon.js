Udon = (typeof Udon === 'undefined') ? {} : Udon;

Udon.ncurry = function(n) {
    if (typeof n != 'number' || n < 2) n = 2;
    return function(f) {
        var a1 = Array.prototype.slice.call(arguments, 1),
        curry = function() {
            var a2 = a1.concat(Array.prototype.slice.call(arguments, 0));
            if (a2.length < n) {
                a2.unshift(f);
                a1 = a2;
                return _curry;
            } else {
                return fn.apply(null, a2);
            }
        };
        return curry;
    };
};

Udon.curry = Udon.ncurry(2);

Udon.foldl = function(f, z, xs) {
    var len = xs.length, i;
    for (i = 0; i < len; i++) z = f(z, xs[i]);
    return z;
};

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
