parser grammar SYsUParser;

options {
  tokenVocab=SYsULexer;
}



// Expressions ------------------------------------------------------------

primaryExpression
    :   Identifier
    |   Numeric_Constant
    |   L_Paren expression R_Paren
    ;

postfixExpression
    :   primaryExpression 
    |   postfixExpression postfixOperator
    ;

// 数组索引时 [] 内必须要有表达式，但数组声明时可以没有
// 函数调用 ()
postfixOperator
    :   L_Square expression R_Square
    |   L_Paren argumentExpressionList? R_Paren
    ;

unaryExpression
    :
    (
        postfixExpression
    |   unaryOperator unaryExpression
    ) 
    ;

unaryOperator
    :   Plus | Minus
    ;

multiplicativeExpression
    :   unaryExpression ((Star|Slash|Percent) unaryExpression)*
    ;

additiveExpression
    :   multiplicativeExpression ((Plus|Minus) multiplicativeExpression)*
    ;

comparativeExpression
    :   additiveExpression ((Less|LessEqual|Greater|GreaterEqual) additiveExpression)*
    ;

equativeExpression
    :   comparativeExpression ((EqualEqual|ExclaimEqual) comparativeExpression)*
    ;

logicalAndExpression
    :   equativeExpression (AmpAmp equativeExpression)*
    ;

logicalOrExpression
    :   logicalAndExpression (PipePipe logicalAndExpression)*
    ;

assignmentExpression
    :   logicalOrExpression
    |   unaryExpression Equal assignmentExpression
    ;



expression
    :   assignmentExpression (Comma assignmentExpression)*
    ;

argumentExpressionList
    :   assignmentExpression (Comma assignmentExpression)*
    ;




// Declaration ------------------------------------------------------------

declaration
    :   declarationSpecifiers initDeclaratorList? Semi
    ;

declarationSpecifiers
    :   declarationQualifier? declarationSpecifier+
    ;

declarationQualifier
    :   typeQualifier
    ;

declarationSpecifier
    :   typeSpecifier
    ;

initDeclaratorList
    :   initDeclarator (Comma initDeclarator)*
    ;

initDeclarator
    :   declarator (Equal initializer)?
    ;


typeQualifier
    :   Const
    ;

typeSpecifier
    :   Int | Void
    ;


declarator
    :   directDeclarator
    ;

directDeclarator
    :   Identifier
    |   directDeclarator L_Square assignmentExpression? R_Square
    ;

identifierList
    :   Identifier (Comma Identifier)*
    ;

initializer
    :   assignmentExpression
    |   L_Brace initializerList? Comma? R_Brace
    ;

initializerList
    // :   designation? initializer (Comma designation? initializer)*
    :   initializer (Comma initializer)*
    ;



// Statements ------------------------------------------------------------

statement
    :   compoundStatement
    |   expressionStatement
    |   conditionStatement
    |   jumpStatement
    ;

compoundStatement
    :   L_Brace blockItemList? R_Brace
    ;

blockItemList
    :   blockItem+
    ;

blockItem
    :   statement
    |   declaration
    ;

expressionStatement
    :   expression? Semi
    ;

conditionStatement
    :   If L_Paren expression R_Paren statement (Else statement)?
    ;

jumpStatement
    :   
    (
        Return expression?
    )
    Semi
    ;



// Global ------------------------------------------------------------

compilationUnit
    :   translationUnit? EOF
    ;

translationUnit
    :   externalDeclaration+
    ;

externalDeclaration
    :   functionDefinition
    // |   functionDeclaration
    |   declaration
    ;

// functionDeclaration
//     :   declarationSpecifiers directDeclarator L_Paren parameterDeclarationList? R_Paren Semi
//     ;

functionDefinition
    :   declarationSpecifiers directDeclarator L_Paren parameterDeclarationList? R_Paren compoundStatement
    ;

parameterDeclarationList
    :   parameterDeclaration (Comma parameterDeclaration)*
    ;

parameterDeclaration
    :   declarationSpecifiers initDeclarator
    ;

