Udon = (typeof Udon === 'undefined') ? {} : Udon;

if (typeof module == 'object') module.exports = Udon;

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

Udon.concat = function(xs) {
    return Udon.foldl1(function(ys, zs) {
        return ys.concat(zs);
    }, xs);
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

Udon.reverse = function(xs) {
    var len = xs.length, ys, i;
    if (len < 1) return xs;
    ys = new Array(len);
    i  = len;
    while (i--) ys[i] = xs[len - i - 1];
    return ys;
};

Udon.intersperse = function(sep, xs) {
    var i = xs.length;
    if (i < 2) return xs;
    i = i * 2 - 1;
    ys = new Array(i);
    while (i--) ys[i] = i % 2 === 0 ? xs[Math.floor(i / 2)] : sep;
    return ys;
};

Udon.intercalate = function(xs, xss) {
    return Udon.concat(Udon.intersperse(xs, xss));
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

Udon.sum = function(ns) {
    var i = ns.length, t = 0;
    while (i--) t += ns[i];
    return t;
};

Udon.product = function(ns) {
    var i = ns.length, t = 1;
    while (i--) t *= ns[i];
    return t;
};

Udon.elem = function(e, xs) {
    var i = xs.length;
    while (i--) if (e === xs[i]) return true;
    return false;
};

Udon.notElem = function(e, xs) {
    var i = xs.length, notInList = true;
    while (i--) if (e === xs[i]) notInList = false;
    return notInList;
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


Udon.and = function(xs) {
    var len, i;
    len = xs.length;

    for (i = 0; i < len; i++) {
        if (xs[i] === false) return false;
    }
    return true;
};

Udon.append = function(xs, ys) {
    return xs.concat(ys);
};

Udon.concatMap = function(f, xs) {
    return Udon.concat(Udon.map(f, xs));
}

Udon.cons = function(x, xs) {
    return [x].concat(xs);
};

Udon.drop = function(n, xs) {
    var len = xs.length;

    if (n > len) return [];

    return Udon._slice.call(xs, n, len);
};

Udon.dropWhile = function(pred, xs) {
    var i, len = xs.length;

    if (pred(xs[0]) === false) return xs;
    
    for (i = 1; i < len; i++) {
        if (pred(xs[i]) === false) {
            return Udon.drop(i, xs);
        }
    }
};

Udon.empty = function(xs) {
    var key;

    if (xs.length && xs.length > 0) return false;

    for (key in xs) {
        if (xs.hasOwnProperty(key)) return false;
    }
    return true;
};

Udon.equal = function(xs, ys) {
    var i, xlen = xs.length, ylen = ys.length;
    if (typeof xs !== typeof ys) {
        return false;
    } else if (typeof xs !== 'object') {
        return xs === ys;
    } 
    
    if (xlen !== ylen) return false;

    for (var i = 0; i < xlen; i++) {
        if (Udon.equal(xs[i], ys[i]) === false) return false;
    }

    return true;
}

Udon.foldr1 = function(f, xs) {
    var len = xs.length, z = xs[len - 1], i;
    for (i = len - 2; i >= 0; i--) z = f(z, xs[i]);
    return z;
};

Udon.head = function(xs) {
    return xs[0];
}

Udon.init = function(xs) {
    return Udon._slice.call(xs, 0, xs.length - 1);
}

Udon.last = function(xs) {
    return xs[xs.length - 1];
}

Udon.length = function(xs) {
    return xs.length;
};

Udon.or = function(xs) {
    var len = xs.length, i;
    for (i = 0; i < len; i++) {
        if (xs[i] === true) return true;
    }
    return false;
};

Udon.permutations = function (xs) {
    var permute = function(perms, used, xs) {
        var i, next, len = xs.length;
        for (i = 0; i < len; i++) {
            next = xs.splice(i, 1)[0];
            used.push(next);
            if (xs.length === 0) {
                perms.push(used.slice());
            }
            permute(perms, used, xs);
            xs.splice(i, 0, next);
            used.pop();
        }
        return perms;
    };

    return permute([], [], xs);
};

Udon.scanl = function(f, q, x_xs) {
    var x, xs;
    if (Udon.empty(x_xs)) return [q];
    x = Udon.head(x_xs);
    xs = Udon.tail(x_xs);
    return Udon.cons(q, Udon.scanl(f, f(q, x), xs));
};

Udon.scanl1 = function(f, x_xs) {
    var x, xs;
    if (Udon.empty(x_xs)) return [];

    x = Udon.head(x_xs);
    xs = Udon.tail(x_xs);
    return Udon.scanl(f, x, xs);
};

Udon.scanr = function(f, q0, x_xs) {
    var q_qs, q, x, xs;
    if (Udon.empty(x_xs)) return [q0];
    
    x = Udon.head(x_xs);
    xs = Udon.tail(x_xs);

    q_qs = Udon.scanr(f, q0, xs);
    q = Udon.head(q_qs);
    return Udon.cons(f(x, q), q_qs);
};

Udon.scanr1 = function(f, x_xs) {
    var lastx, len = x_xs.length;
    if (Udon.empty(x_xs)) return [];
    lastx = Udon.last(x_xs);

    return Udon.scanr(f, lastx, Udon._slice.call(x_xs, 0, len - 1));
};    

Udon.splitAt = function(n, xs) {
    return [Udon.take(n, xs), Udon.drop(n, xs)];
};

Udon.subsequences = function(xs) {
    var col = [];

    var nonEmptySubs = function(xs) {
        var x;
        if (Udon.empty(xs)) return xs;
        x = Udon.head(xs);

        var f = function(ys, r) {
            return Udon.cons(ys, Udon.cons(Udon.cons(x, ys), r));
        };

        return Udon.cons([x], Udon.foldr(f, [], nonEmptySubs(Udon.tail(xs))));
    };

    return Udon.cons([], nonEmptySubs(xs));
};

Udon.tail = function(xs) {
    return Udon._slice.call(xs, 1, xs.length);
}

Udon.take = function(n, xs) {
    var len = xs.len;
    if (n > len) return xs;

    return Udon._slice.call(xs, 0, n);
};

Udon.takeWhile = function(pred, xs) {
    var i, len = xs.length;

    if (pred(xs[0]) === false) return [];
    
    for (i = 1; i < len; i++) {
        if (pred(xs[i]) === false) {
            return Udon.take(i, xs);
        }
    }
};


Udon.transpose = function(xs) {
    var head_len, total_len, i, j, col = [];
    head_len = Udon.maximum(Udon.map(Udon.length, xs));
    total_len = xs.length;

    if (Udon.empty(xs)) return xs;

    for (i = 0; i < head_len; i++) {
        col.push([]);
        for (j = 0; j < total_len; j++) {
            if (typeof xs[j][i] !== 'undefined') col[i].push(xs[j][i]);
        }
    }
    return col;
};
    