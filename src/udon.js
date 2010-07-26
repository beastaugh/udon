Udon = (typeof Udon === 'undefined') ? {} : Udon;

Udon.foldl = function(fn, z, xs) {
    var len = xs.length, i;
    for (i = 0; i < len; i++) z = fn(z, xs[i]);
    return z;
};

Udon.foldr = function(fn, z, xs) {
    var i = xs.length;
    while (i--) z = fn(xs[i], z);
    return z;
};

Udon.map = function(fn, xs) {
    var i = xs.length, ys = new Array(i);
    while (i--) ys[i] = fn(xs[i]);
    return ys;
};

Udon.filter = function(fn, xs) {
    var ys = [], len = xs.length, i, e;
    for (i = 0; i < len; i++) {
        e = xs[i];
        if (fn(e)) ys.push(e);
    }
    return ys;
};

Udon.partition = function(fn, xs) {
    var ts = [], fs = [], len = xs.length, i, e;
    for (i = 0; i < len; i++) {
        e = xs[i];
        (fn(e) ? ts : fs).push(e);
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

Udon.zipWith = function(fn, xs, ys) {
    var xsl = xs.length, ysl = ys.length, i = xsl > ysl ? ysl : xsl,
        zs = new Array(i);
    while (i--) zs[i] = fn(xs[i], ys[i]);
    return zs;
};
