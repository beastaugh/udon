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
    var ys = [], i = xs.length;
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

Udon.unfoldr = function(step, seed) {
    var output  = [], result;
    while (result = step(seed)) {
        output.push(result[0]);
        seed = result[1];
    }
    return output;
};
