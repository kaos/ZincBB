function field_default(f, text) 
{
    var f = $(f);
    f.attr('value', text);
    f.focus(function() {
	    if (f.attr('value') == text)
		f.attr('value', '');
	});
    f.blur(function() {
	    if (f.attr('value') == '')
		f.attr('value', text);
	});
}


function modal_box(content) 
{
    $(content).load(function() {
	    new Boxy(content, {title:"ZincBB", modal:true});
	});
}
