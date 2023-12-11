use Cro::WebApp::Template::Error;

class X::Cro::WebApp::Template::SyntaxError does X::Cro::WebApp::Template {
    has Str $.reason is required;
    has Cursor $.cursor is required;

    method message() {
        "Template parse failed: $!reason at line $.line near '$.near'"
    }

    method line() {
        $!cursor.orig.substr(0, $!cursor.pos).split(/\n/).elems
    }

    method near() {
        $!cursor.orig.substr($!cursor.pos, 40)
    }
}

my ($S-O, $S-C, $S-OC) = ( "'<'", "'>'", "'</'" );
sub sequence-start () is rw is export {
  Proxy.new:
    FETCH => -> $     { $S-O },
    STORE => -> $, \v { $S-O = v }
}

sub sequence-end () is rw is export {
  Proxy.new:
    FETCH => -> $     { $S-C },
    STORE => -> $, \v { $S-C = v }
}

sub set-single-tag-start ($oc) is export {
  $*S-OC = $oc;
}

grammar Cro::WebApp::Template::Parser {
    token TOP {
        :my $*IN-ATTRIBUTE = False;
        :my $*IN-MACRO = False;
        :my @*USED-FILES;
        <sequence-element>*
        [ $ || <.panic: 'confused'> ]
    }

    proto token sequence-element($*TAG-CONTEXT = '') { * }

    token sequence-element:sym<sigil-tag> {
        <sigil-tag>
    }

    token sequence-element:sym<literal-text> {
        <-[<]>+
    }

    token sequence-element:sym<literal-open-tag> {
        :my $*IN-ATTRIBUTE = True;
        <{ $*S-O }> <![/]> <!sigil>
        <tag-element>+
        [ <{ $*S-C }> || <.panic: "malformed tag"> ]
    }

    token sequence-element:sym<literal-close-tag> {
        <{ $*S-OC }> <!sigil>
        <-[>]>+
        [ <{ $*S-C }> || <.panic: "malformed closing tag"> ]
    }



    proto token tag-element { * }

    token tag-element:sym<sigil-tag> {
        <sigil-tag>
    }

    token sigil-tag:sym<comment> {
      <{ $*S-O }> '#' <{ $*S-C }> .+? <{ $*S-O }> '/#' <{ $*S-C }>
    }

    token tag-element:sym<literal> {
        | '!--' .*? '--' <?before <{ $*S-C }>>
        | <-[<>]>+
    }

    proto token sigil-tag { * }

    token sigil-tag:sym<geLiteral> { <$*S-O> '\ge' <{ $*S-C }> }
    token sigil-tag:sym<leLiteral> { <$*S-O> '\le' <{ $*S-C }> }
    token sigil-tag:sym<gtLiteral> { <$*S-O> '\gt' <{ $*S-C }> }
    token sigil-tag:sym<ltLiteral> { <$*S-O> '\lt' <{ $*S-C }> }

    token sigil-tag:sym<topic> {
        <{ $*S-O }> '.'
        [ <deref> || <.malformed: 'topic tag'> ]
        [ <{ $*S-C }> || <.malformed: 'topic tag'> ]
    }

    token sigil-tag:sym<variable> {
        <{ $*S-O }> '$'
        [ <identifier> || <.malformed: 'variable tag'> ]
        [ '.' <deref> ]?
        [ <{ $*S-C }> || <.malformed: 'variable tag'> ]
    }

    token sigil-tag:sym<iteration> {
        :my $*SEPARATOR;
        :my $opener = $¢.clone;
        :my $*lone-start-line = False;
        <{ $*S-O }> '@'
        [ <?after [^ | $ | \n] \h* <{ $*S-O }> '@'> { $*lone-start-line = True } ]?
        [
        | '.'? <deref>
        | $<variable>=['$' <.identifier>] ['.' <deref>]?
        || <.malformed: 'iteration tag'>
        ]
        [\h* ':' \h* <iteration-variable=.parameter(:!allow-named, :!allow-default)>]?
        [ \h+ <structural-tag> ]?
        [ \h* <{ $*S-C }> || <.malformed: 'iteration tag'> ]
        [ <?{ $*lone-start-line }> [ \h* \n | { $*lone-start-line = False } ] ]?

        <sequence-element('iteration')>*

        :my $*lone-end-line = False;
        [ <{ $*S-O }> '/@' || { $opener.unclosed('iteration tag') } ]
        [ <?after \n \h* <{ $*S-O }> '/@'> { $*lone-end-line = True } ]?
        <close-ident=.ident>?
        [ \h* <{ $*S-C }> || <.malformed: 'iteration closing tag'> ]
        [ <?{ $*lone-end-line }> [ \h* \n | { $*lone-end-line = False } ] ]?
    }

    token sigil-tag:sym<separator> {
        :my $opener = $¢.clone;
        :my $*lone-start-line = False;
        '<:separator'
        [ <?after [^ | $ | \n] \h* '<:separator'> { $*lone-start-line = True } ]?
        \h*
        [
        || <{ $*S-C }>
        || <.malformed: 'separator tag'>
        ]
        [ <?{ $*lone-start-line }> [ \h* \n | { $*lone-start-line = False } ] ]?

        [
        || <?{ $*TAG-CONTEXT eq 'iteration' }>
        || <.panic('separator may only appear inside of an iteration')>
        ]
        [
        || <!{ $*SEPARATOR }>
        || <.panic('separator for this iteration already defined')>
        ]

        <sequence-element>*

        :my $*lone-end-line = False;
        [ <{ $*S-O }> '/:' || { $opener.unclosed('separator tag') } ]
        [ <?after \n \h* <{ $*S-O }> '/:'> { $*lone-end-line = True } ]?
        [ 'separator'? \h* <{ $*S-C }> || <.malformed: 'separator closing tag'> ]
        [ <?{ $*lone-end-line }> [ \h* \n | { $*lone-end-line = False } ] ]?
    }

    token sigil-tag:sym<condition> {
        <!before <{ $*S-O }>'!-' >
        :my $opener = $¢.clone;
        :my $*lone-start-line = False;
        <{ $*S-O }> $<negate>=<[?!]>
        [ <?after [^ | $ | \n] \h* <{ $*S-O }> <[?!]>> { $*lone-start-line = True } ]?
        <condition>
        [ \h+ <structural-tag> ]?
        [ \h* <{ $*S-C }> || <.malformed: 'condition tag'> ]
        [ <?{ $*lone-start-line }> [ \h* \n | { $*lone-start-line = False } ] ]?

        <sequence-element>*

        :my $*lone-end-line = False;
        [ <{ $*S-O }> '/' $<negate> || { $opener.unclosed('condition tag') } ]
        [ <?after \n \h* <{ $*S-O }> '/' <[?!]>> { $*lone-end-line = True } ]?
        <close-ident=.ident>?
        [ \h* <{ $*S-C }> || <.malformed: 'condition closing tag'> ]
        [ <?{ $*lone-end-line }> [ \h* \n | { $*lone-end-line = False } ] ]?

        [
        || <?before \s* <{ $*S-O }> '!?'>
           [ { $<negate> eq '?' } || <.panic('cannot have elsif parts on a negated conditional tag')> ]
           \s* <else=.elsif>
        || <?before \s* <{ $*S-O }> '!' [\s | \h* <{ $*S-C }>]>
           [ { $<negate> eq '?' } || <.panic('cannot have else parts on a negated conditional tag')> ]
           \s* <else>
        ]?
    }

    token elsif {
        :my $opener = $¢.clone;
        :my $*lone-start-line = False;
        <{ $*S-O }> '!?'
        [ <?after [^ | $ | \n] \h* <{ $*S-O }> '!?'> { $*lone-start-line = True } ]?
        <condition>
        [ \h+ <structural-tag> ]?
        [ \h* <{ $*S-C }> || <.malformed: 'condition tag'> ]
        [ <?{ $*lone-start-line }> [ \h* \n | { $*lone-start-line = False } ] ]?

        <sequence-element>*

        :my $*lone-end-line = False;
        [ <{ $*S-O }> '/?' <{ $*S-C }> || { $opener.unclosed('condition tag') } ]
        [ <?after \n \h* <{ $*S-O }> '/?' <{ $*S-C }>> { $*lone-end-line = True } ]?
        [ <?{ $*lone-end-line }> [ \h* \n | { $*lone-end-line = False } ] ]?

        [
        || <?before \s* <{ $*S-O }> '!?'>
           \s* <else=.elsif>
        || <?before \s* <{ $*S-O }> '!' [\s | \h* <{ $*S-C }>]>
           \s* <else>
        ]?
    }

    token else {
        :my $opener = $¢.clone;
        :my $*lone-start-line = False;
        <{ $*S-O }>'!'
        [ <?after [^ | $ | \n] \h* <{ $*S-O }>'!'> { $*lone-start-line = True } ]?
        [ \h+ <structural-tag> ]?
        [ \h* <{ $*S-C }> || <.malformed: 'condition tag'> ]
        [ <?{ $*lone-start-line }> [ \h* \n | { $*lone-start-line = False } ] ]?

        <sequence-element>*

        :my $*lone-end-line = False;
        [ <{ $*S-O }> '/!' <{ $*S-C }> || { $opener.unclosed('condition tag') } ]
        [ <?after \n \h* <{ $*S-O }> '/!' <{ $*S-C }>> { $*lone-end-line = True } ]?
        [ <?{ $*lone-end-line }> [ \h* \n | { $*lone-end-line = False } ] ]?
    }

    token condition {
        | '.' <deref>
        | '$' <identifier> ['.' <deref>]?
        | '{' <expression> [ '}' || <.panic('malformed expression')> ]
        || <.malformed: 'condition tag'>
    }

    token structural-tag {
        <tag=.ident>
        :my $*IN-ATTRIBUTE = True;
        <tag-element>*
    }

    token sigil-tag:sym<call> {
        <{ $*S-O }> '&'
        [
        || <target=.identifier> \h* <arglist>? \h*
        || <.malformed: 'call tag'>
        ]
        [ <{ $*S-C }> || <.malformed: 'call tag'> ]
    }

    token sigil-tag:sym<sub> {
        :my $opener = $¢.clone;
        :my $*lone-start-line = False;
        <{ $*S-O }> ':sub'
        [ <?after [^ | $ | \n] \h* <{ $*S-O }> ':sub'> { $*lone-start-line = True } ]?
        \h+
        [
        || <name=.identifier> \h* <signature>? <{ $*S-C }>
        || <.malformed: 'sub declaration tag'>
        ]
        [ <?{ $*lone-start-line }> [ \h* \n | { $*lone-start-line = False } ] ]?

        <sequence-element>*

        :my $*lone-end-line = False;
        [ <{ $*S-O }> '/:' || { $opener.unclosed('sub declaration tag') } ]
        [ <?after \n \h* <{ $*S-O }>'/:'> { $*lone-end-line = True } ]?
        [ 'sub'? \h* <{ $*S-C }> || <.malformed: 'sub declaration closing tag'> ]
        [ <?{ $*lone-end-line }> [ \h* \n | { $*lone-end-line = False } ] ]?
    }

    token sigil-tag:sym<macro> {
        :my $opener = $¢.clone;
        :my $*lone-start-line = False;
        <{ $*S-O }> ':macro'
        [ <?after [^ | $ | \n] \h* <{ $*S-O }> ':macro'> { $*lone-start-line = True } ]?
        \h+
        [
        || <name=.identifier> \h* <signature>? <{ $*S-C }>
        || <.maformed: 'macro declaration tag'>
        ]
        [ <?{ $*lone-start-line }> [ \h* \n | { $*lone-start-line = False } ] ]?

        :my $*IN-MACRO = True;
        <sequence-element>*

        :my $*lone-end-line = False;
        [ <{ $*S-O }> '/:' || { $opener.unclosed('macro declaration tag') } ]
        [ <?after \n \h* <{ $*S-O }> '/:'> { $*lone-end-line = True } ]?
        [ 'macro'? \h* <{ $*S-C }> || <.malformed: 'macro declaration closing tag'> ]
        [ <?{ $*lone-end-line }> [ \h* \n | { $*lone-end-line = False } ] ]?
    }

    token sigil-tag:sym<body> {
        [{ $*IN-MACRO } || <.panic('Use of <:body> outside of a macro')>]
        <{ $*S-O }> ':body' \h* <{ $*S-C }>
    }

    token sigil-tag:sym<part> {
        :my $opener = $¢.clone;
        :my $*lone-start-line = False;
        <{ $*S-O }> ':part'
        [ <?after [^ | $ | \n] \h* <{ $*S-O }> ':part'> { $*lone-start-line = True } ]?
        \h+
        [
        || <name=.identifier> \h* <signature>? <{ $*S-C }>
        || <.malformed: 'part declaration tag'>
        ]
        [ <?{ $*lone-start-line }> [ \h* \n | { $*lone-start-line = False } ] ]?

        <sequence-element>*

        :my $*lone-end-line = False;
        [ <{ $*S-O }> '/:' || { $opener.unclosed('part declaration tag') } ]
        [ <?after \n \h* <{ $*S-O }> '/:'> { $*lone-end-line = True } ]?
        [ 'part'? \h* <{ $*S-C }> || <.malformed: 'part declaration closing tag'> ]
        [ <?{ $*lone-end-line }> [ \h* \n | { $*lone-end-line = False } ] ]?
    }

    token sigil-tag:sym<apply> {
        :my $*lone-start-line = False;
        <{ $*S-O }> '|'
        [ <?after [^ | $ | \n] \h* <{ $*S-O }> '|'> { $*lone-start-line = True } ]?
        [
        || <target=.identifier> \h* <arglist>? \h* <{ $*S-C }>
        || <.malformed: 'macro application tag'>
        ]
        [ <?{ $*lone-start-line }> [ \h* \n | { $*lone-start-line = False } ] ]?

        <sequence-element>*

        :my $*lone-end-line = False;
        <{ $*S-O }> '/|'
        [ <?after \n \h* <{ $*S-O }> '/|'> { $*lone-end-line = True } ]?
        <close-ident=.ident>?
        [ \h* <{ $*S-C }> || <.malformed: 'macro application closing tag'> ]
        [ <?{ $*lone-end-line }> [ \h* \n | { $*lone-end-line = False } ] ]?
    }

    token sigil-tag:sym<use> {
        <{ $*S-O }> ':use' \h+
        [
        | <file=.single-quote-string>
        | <library=.module-name>
        || <.malformed: 'use tag'>
        ]
        \h* <{ $*S-C }>
    }

    token module-name {
        <.identifier>+ % '::'
    }

    token signature {
        :my $*seen-by-name-arguments = False;
        '(' \s* <parameter>* % [\s* ',' \s*] \s*
        [ ')' || <.malformed: 'signature'> ] \h*
    }

    token parameter(:$allow-named = True, :$allow-default = True) {
        [
        || $<named>=':'
           [ <?{ $allow-named }> || <.panic('Canot use a named parameter here')> ]
           { $*seen-by-name-arguments = True; }
        || <?{ $*seen-by-name-arguments }> <.panic('Positional argument after named argument')>
        ]?
        $<name>=['$' <.identifier>]
        [ <?{ $allow-default }> \s* '=' \s* <default=.expression> ]?
    }

    token arglist {
        '(' \s* <arg>* % [\s* ',' \s*] \s* ')' \h*
    }

    proto token arg { * }

    token arg:by-pos { <expression> }

    token arg:by-name {
        :my $negated = False;
        ':'
        [
        | $<var-name>=['$' <identifier>]
        | [ $<negated>='!' { $negated = True} ]?
          <identifier>
          [
          '(' ~ ')' <expression>
          [ <!{$negated}> || <.panic('Negated named argument may not have a value')> ]
          ]?
        ]


    }

    rule expression {
        <!before ')'>
        [ <term> || <.panic('unrecognized term')> ]
        [ <infix> [ <term> || <.panic('missing or unrecognized term')> ] ]*
    }

    proto token term { * }

    token term:sym<single-quote-string> {
        <single-quote-string>
    }

    token term:sym<integer> {
        '-'? \d+
    }

    token term:sym<rational> {
        '-'? \d* '.' \d+
    }

    token term:sym<num> {
        '-'? \d* '.' \d+ <[eE]> '-'? \d+
    }

    token term:sym<bool> {
        True | False
    }

    token term:sym<variable> {
        $<name>=[ '$' <.identifier> ] [ '.' <deref> ]?
    }

    token term:sym<deref> {
        '.' <deref>
    }

    rule term:sym<parens> { '(' <expression> ')' }

    proto token infix { * }
    token infix:sym<==> { <sym> }
    token infix:sym<!=> { <sym> }
    token infix:sym<< < >> { <sym> }
    token infix:sym<< <= >> { <sym> }
    token infix:sym<< > >> { <sym> }
    token infix:sym<< >= >> { <sym> }
    token infix:sym<eq> { <sym> }
    token infix:sym<ne> { <sym> }
    token infix:sym<lt> { <sym> }
    token infix:sym<gt> { <sym> }
    token infix:sym<le> { <sym> }
    token infix:sym<ge> { <sym> }
    token infix:sym<===> { <sym> }
    token infix:sym<!===> { <sym> }
    token infix:sym<&&> { <sym> }
    token infix:sym<||> { <sym> }
    token infix:sym<and> { <sym> }
    token infix:sym<or> { <sym> }
    token infix:sym<+> { <sym> }
    token infix:sym<-> { <sym> }
    token infix:sym<*> { <sym> }
    token infix:sym</> { <sym> }
    token infix:sym<%> { <sym> }
    token infix:sym<~> { <sym> }
    token infix:sym<x> { <sym> }

    token deref {
        <deref-item>+ % '.'
    }

    proto token deref-item { * }
    token deref-item:sym<method> {
        <identifier> <arglist>
    }
    token deref-item:sym<smart> {
        <.identifier>
    }
    token deref-item:sym<hash-literal> {
        '<' <( <-[>]>* )> '>'
    }
    token deref-item:sym<array> {
        '[' <index=.expression> ']'
    }
    token deref-item:sym<hash> {
        '{' <key=.expression> '}'
    }

    token single-quote-string {
        "'" <( <-[']>* )> "'"
    }

    token sigil {
        # Single characters we can always take as a tag sigil
        | <[.$@&:|]>
        # The ? and ! for boolification must be followed by a . or $ tag sigil or
        # { expression. <!DOCTYPE>, <?xml>, and <!--comment--> style things
        # must be considered literal.
        | <[?!]> <[.$>{]>
    }

    token identifier {
        <.ident> [ <[-']> <.ident> ]*
    }

    method malformed($what) {
        self.panic("malformed $what")
    }

    method unclosed($what) {
        self.panic("unclosed $what")
    }

    method panic($reason) {
        die X::Cro::WebApp::Template::SyntaxError.new:
                :$reason, :cursor(self), :file($*TEMPLATE-FILE // '<unknown>'.IO);
    }
}
