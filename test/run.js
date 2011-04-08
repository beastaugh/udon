JSCLASS_PATH = 'vendor/jsclass';

if (typeof require == 'function') {
    require('../' + JSCLASS_PATH + '/loader');
} else {
    load(JSCLASS_PATH + '/loader.js');
}

JS.Packages(function() { with(this) {
    autoload(/^(.*)Spec$/, {
        from: 'test/specs',
        require: '$1'});
    
    file('src/udon.js')
        .provides('Udon')
        .requires('JS.Class');
}});

JS.require('JS.Test', function() {
    JS.require('UdonSpec', JS.Test.method('autorun'));
});
