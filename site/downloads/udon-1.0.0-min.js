Udon=(typeof Udon==='undefined')?{}:Udon;Udon._0=Array.prototype.slice;Udon.ncurry=function(b){if(typeof b!='number'||b<2)b=2;var e=function(a){var d=Udon._0.call(arguments,1);return function(){var c=d.concat(Udon._0.call(arguments,0));if(c.length<b){c.unshift(a);return e.apply(null,c)}else{return a.apply(null,c)}}};return e};Udon.curry=function(a){var d=a.length,b=Udon._0.call(arguments,1),e=function(){var c=b.concat(Udon._0.call(arguments,0));return c.length>=d?a.apply(null,c):Udon.curry.apply(null,[a].concat(c))};return b.length>=d?e():e};Udon.compose=function(b,e){var f=function(){var c=b.length-1,a,d=Udon._0.call(arguments,0);if(c<0)return d[0];a=b[c].apply(null,d);while(c--)a=b[c](a);return a};return typeof e=='number'&&e>1?Udon.ncurry(e)(f):f};Udon.foldl=function(c,a,d){var b=d.length,e;for(e=0;e<b;e++)a=c(a,d[e]);return a};Udon.reduce=Udon.foldl;Udon.inject=Udon.foldl;Udon.foldr=function(c,a,d){var b=d.length;while(b--)a=c(d[b],a);return a};Udon.map=function(c,a){var d=a.length,b=new Array(d);while(d--)b[d]=c(a[d]);return b};Udon.filter=function(c,a){var d=[],b=a.length,e,f;for(e=0;e<b;e++){f=a[e];if(c(f))d.push(f)}return d};Udon.select=Udon.filter;Udon.any=function(c,a){var d=a.length;while(d--)if(c(a[d]))return true;return false};Udon.all=function(c,a){var d=a.length;while(d--)if(!c(a[d]))return false;return true};Udon.none=function(c,a){return!Udon.any(c,a)};Udon.partition=function(c,a){var d=[],b=[],e=a.length,f,g;for(f=0;f<e;f++){g=a[f];(c(g)?d:b).push(g)}return[d,b]};Udon.unfoldr=function(c,a){var d=[],b;while((b=c(a))){d.push(b[0]);a=b[1]}return d};Udon.zip=function(d,b){return Udon.zipWith(function(c,a){return[c,a]},d,b)};Udon.zipWith=function(c,a,d){var b=a.length,e=d.length,f=b>e?e:b,g=new Array(f);while(f--)g[f]=c(a[f],d[f]);return g};Udon.max=function(d,b){var b=(b==undefined)?function(c){return c}:b;return Udon.foldl(function(c,a){return b(c)>b(a)?c:a},0,d)}Udon.min=function(d,b){var b=(b==undefined)?function(c){return c}:b;return Udon.foldl(function(c,a){return b(c)<b(a)?c:a},Infinity,d)}