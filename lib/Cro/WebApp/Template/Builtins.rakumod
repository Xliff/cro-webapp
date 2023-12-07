class X::Cro::WebApp::Template::XSS is Exception {
    has Str $.content is required;
    method message() {
        "Potential XSS attack detected in content '$!content' being rendered using <\&HTML(...)> in template"
    }
}

sub __TEMPLATE_SUB__HTML(Str() $html) is export {
    if $html ~~ /:i '<' \s* script \W | \" \s* javascript \s* ':'/ {
        die X::Cro::WebApp::Template::XSS.new(content => ~$/);
    }
    $html
}

sub __TEMPLATE_SUB__HTML-AND-JAVASCRIPT(Str() $html) is export {
    $html
}

sub __TEMPLATE_SUB__GENERATE-FORM-PREFIX(Str() $p, Str() $n) is export {
  "{ $p ?? "{ $p }-" !! '' }{ $n }";
}

sub __TEMPLATE_SUB__MERGESPACES(Str() $text) is export {
    $text.subst(/\s+/, ' ', :g);
}
