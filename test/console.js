(function() {
    var $ = (typeof global === 'object') ? global : this;
    $.JSCLASS_PATH = 'vendor/jsclass';
})();

if (typeof require === 'function') {
    require('../' + JSCLASS_PATH + '/loader');
    require('./run');
} else {
    load(JSCLASS_PATH + '/loader.js');
    load('test/run.js');
}
