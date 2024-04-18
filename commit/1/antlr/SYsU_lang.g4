lexer grammar SYsU_lang;

Int : 'int';
Return : 'return';
Const : 'const';
If : 'if';
Else : 'else';
Void : 'void';
While : 'while';
Break : 'break';
Continue: 'continue';

L_Paren : '(';
R_Paren : ')';
L_Square : '[';
R_Square : ']';
L_Brace : '{';
R_Brace : '}';

Plus : '+';
Minus : '-';
Star : '*';
Slash : '/';
Percent : '%';

Semi : ';';
Comma : ',';

Equal : '=';
EqualEqual : '==';
ExclaimEqual : '!=';
Greater : '>';
GreaterEqual : '>=';
Less : '<';
LessEqual : '<=';

Exclaim : '!';
Pipe : '|';
Amp : '&';
PipePipe : '||';
AmpAmp : '&&';

Identifier
    :   IdentifierNondigit
        (   IdentifierNondigit
        |   Digit
        )*
    ;

fragment IdentifierNondigit
    :   Nondigit
    ;

fragment Nondigit
    :   [a-zA-Z_]
    ;

fragment Digit
    :   [0-9]
    ;

Numeric_Constant
    :   IntegerConstant
    ;

fragment IntegerConstant
    :   DecimalConstant
    |   OctalConstant
    |   HexadecimalConstant
    ;

fragment DecimalConstant
    :   NonzeroDigit Digit*
    ;

fragment OctalConstant
    :   '0' OctalDigit*
    ;

fragment HexadecimalConstant
    :   ('0x' [1-9a-f] [0-9a-f]*)
    |   ('0X' [1-9A-F] [0-9A-F]*)
    ;


fragment NonzeroDigit
    :   [1-9]
    ;

fragment OctalDigit
    :   [0-7]
    ;

// 预处理信息处理，可以从预处理信息中获得文件名以及行号
// 预处理信息前面的数组即行号
LineAfterPreprocessing
    :   '#' Whitespace* ~[\r\n]*
    ;

Whitespace
    :   [ \t]+
    ;

// 换行符号，可以利用这个信息来更新行号
Newline
    :   (   '\r' '\n'?
        |   '\n'
        )
    ;

