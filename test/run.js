var CWD = (typeof CWD === 'undefined') ? '.' : CWD;

JS.Packages(function() { with(this) {
    autoload(/^(.*)Spec$/, {
        from: CWD + '/test/specs',
        require: '$1'});
    
    file(CWD + '/src/udon.js')
        .provides('Udon')
        .requires('JS.Class');
}});

JS.require('JS.Test', function() {
    JS.require('UdonSpec', JS.Test.method('autorun'));
});
