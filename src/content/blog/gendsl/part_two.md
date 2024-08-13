---
author: ccbhj
pubDatetime: 2024-08-14T14:01:00Z
modDatetime: 2024-08-14T14:01:00Z
title: "Part II: Implement an expression interpreter for building DSL - Syntax implementation"
slug: "part-two-implement-an-expression-interpreter-for-building-DSL"
featured: true
draft: false
tags:
  - gendsl
  - golang
  - DSL
description: "Get more deep into the implement of the S-Expression and gendsl syntax."
---

## Table of contents

In the last post, I had introduced you the basis of my tiny gendsl project - the PEG parser. So in this post, I gonna introduce you how to implement the syntax. 
If you have read the first post of this series, you may remember my DSL syntax is actually kind of like lisp style syntax, which means a script is consist of [S-Expression](https://en.wikipedia.org/wiki/S-expression). Therefore, let me start with some introduction to the powerful S-Expression.


## Introduce the S-Expression
The S-Expression, or Symbolic Expressions, is well-known for the famous, ancient programming language Lisp(stands for LISt Processor), which was invented in the late 1950s by John McCarthy. It's quite an antique language like Fortran but still in use and got derived by a lot of relatively new language like [Racket](https://racket-lang.org/), [Clojure](https://clojure.org/) and so on. Still, we can pick up a lof of advantage and wisdom from this ancient treasure.
The S-Expression is extremely simple and abstract. In short, a S-Expression is either:
   + an atom
   + an expression of the form `(X . Y)`,  where X and Y are both S-Expression.
An atom is the basic unit that consists of the S-Expression, thus, it could be an integer, a string or a symbol that cannot be expressed by S-Expression. And for the second case it forms a list or in the Lisp way, a [cons cell](https://en.wikipedia.org/wiki/Cons). All the data, all the code is expressed in S-Expression, for example, a list `(1 . (2 . (3 NIL)))` refers to a list `[1, 2, 3, 4]` where the `.` could be ignored in modern  `NIL` is the special end-of-list symbol so that it can be expressed recursively, and `(+ 1 2)` refers to `1 + 2` in more programming language, `(+ 1 (+ 2 3))` nested expression is also supported for sure. In the example of `(+ 1 (+ 2 3))` we treat the plus as a function or procedure if you will and all the other values after `+` are arguments of this function. .
You may notice that in this form of expression we are actually creating a tree instead of a list since a nested list is actually a tree and atoms are just leaves of this tree. Yes, the S-Expression is actually a representation of an abstract syntax tree. Pretty cool, right? That is the beauty of it, and it gives us the power of manipulate the AST easily.

## Implement the S-expression
Now that we have learned the S-Expression syntax, let's implement it with the powerful PEG parser. Still most of the syntax code is copied from my gendsl project(you can check it out [here](https://github.com/ccbhj/gendsl/blob/main/grammar.peg)), and we gonna use the same workspace of the [last blog](https://dev.to/ccbhj/part-i-implement-an-expression-interpreter-for-building-dsl-introduce-the-peg-parser-omf).

### Atomic value
First thing first, we are gonna implement the atomic value which could be an integer, string, boolean or float point number. In this part we will implement most of the lexer work, to classify text into string or number literal.

#### NIL literal
Nothing can be simpler than nil:
```
// peg_playground/grammar.peg

NilLiteral     <-    'nil'
```

#### Boolean literal
An boolean value in gendsl is `#t`(true) of `#f`(false), just like the one in [Racket](https://docs.racket-lang.org/reference/booleans.html). It is pretty easy to implement boolean since it just simply consist of two characters. So in our peg_playground/grammar.peg, there we go:
```
BoolLiteral    <-    ('#f' / '#t') !LetterOrDigit 

LetterOrDigit  <-    [a-z] / [A-Z] / [0-9] / [_]
```
Pretty easy, right? It should be able to match `#t` or `#f`, even `#t)` when it is enclosed with paren, but not `#t1` nor `#tabc`.

#### Integer/Float literal
In face, we had already take integer as the example in the last post. We support decimal integers as well as hexcial. Besides, I also add the support for "_" as seperator, which is also supported in golang. In additional, I also add support for the 'u' or 'U' suffix to support the unsigned integer declaration which adapts from the c/c++. 
```
IntegerLiteral   <-    [+\-]? ('0' ('x' / 'X') HexNumeral / DecimalNumeral ) [uU]?

HexNumeral       <-    HexDigit ([_]* HexDigit)* / '0'

DecimalNumeral   <-    [1-9] ([_]* [0-9])* / '0'

HexDigit         <-    [0-9] / [A-F] / [a-f]
```

The float point number literal is also as simple as the integer literal. But the exponet is a little tricky, since we gonna support formats like '1.1', '1e1', '.1e1' and '.1e-1'.
```
FloatLiteral     <-               [+\-]? (Digits '.' Digits?  Exponent?
                                          /  Digits Exponent
                                          / '.' Digits Exponent?)

Exponent         <-               [eE] [+\-]? Digits

Digits           <-               [0-9]([_]*[0-9])*
```

#### String literal
The string literal is the most complicate part as far as I concern, for there could be a lot of uncertainty in it. But luckily there are answers in [Golang Language Spec](https://go.dev/ref/spec#String_literals). For string literal, we support normal ASCII characters('a', 'b', 'c') and escape sequence('\n') for sure, besides we also support hex byte sequence and unicode sequence.
```
StringLiteral          <-               ["] StringChar* ["]

StringChar             <-               UChar / Escape / HexByte / ![\"\n\\] . 

Escape                 <-               '\\' [abfnrtv\\"']

HexByte                <-               '\\' 'x' HexDigit HexDigit

UChar                  <-               '\\' 'u' HexDigit HexDigit HexDigit HexDigit
                                        /  '\\' 'U' HexDigit HexDigit HexDigit HexDigit HexDigit HexDigit HexDigit HexDigit
```
You should notice that we don't allow new line('\n') in a string literal(`![\"\n\\]`) as most of the languages do. To support string that allows new lines, in other word, the long string that quoteds by '`' in golang and '"""' or "'''" in python, we should add a rule that allow new line characters and unescaped characters: 
```
LongStringLiteral      <-               '"""' LongStringChar* '"""'

LongStringChar         <-               ![\"].
```
The LongStringLiteral is much more simple that the normal StringLiteral. I choose the '"""' quote here just like the python, since I have always thoughtt the '`' in golang is too implict. You might miss it without any highlight suuport in your editor.

#### Identifier
Last but not least, we gonna implement the identifier. An identifier cannot be a boolean(#t/#f), and it can only start with letters or any one character of `[~!@$%^&*_?|<>]` and end with spacing or none. So here we seperate the prefix(IdentifierPrefix) and the rest part(IdentifierChar) of an identifier:
```
Identifier             <-               !BoolLiteral IdentifierPrefix IdentifierChar* Spacing 
                                
IdentifierPrefix       <-               Letter / [~!@$%^&*_?|<>]

Letter                 <-               [a-z] / [A-Z] / [_]
                                
IdentifierChar         <-               LetterOrDigit / [~!@$%^&*_?|<>] / '-'

Spacing                <-               ( [ \t\r\n]+
                                         / ';' (![\r\n] .)* [\r\n])*
```
The spacing rule here is kind of complicate. A spacing should might be one or more whitespace(' ') or newline('\r\n') or line comment that start with a ';' which will be ignored completely.

### Form
As we have discussed before, a S-Expression is either a atom value or a form of `(X Y ...)`, where X and Y are another form or atom value. So we are gonna implement the second part of the S-Expression, a form. 
```
Value           <-         (Expression
                             / Literal
                             / Identifier
                            ) Spacing

Expression      <-         LPAR Operator Value* RPAR 

LPAR            <-          Spacing '(' Spacing

RPAR            <-          Spacing ')' Spacing

Operator         <-         Identifier Spacing?

Literal         <-         ( NilLiteral
                             / FloatLiteral
                             / IntegerLiteral          # May be a prefix of FloatLiteral
                             / LongStringLiteral
                             / StringLiteral
                             / BoolLiteral
                           )  Spacing
```
A form or expression consists of two parts when used as an expression, first part is an operator which is just basically an Identifier, the other are some operands which are some Values and of course there could be no operand at all. What's more important, **a Value could be another a value, or a literal, or just an identifier which is the key to construct a recursive syntax tree for an expression where the operator is the root and operands are its children that might be a literal/identifier(leaf) or another expression(subtree).**

You may have notice the comment I make in the grammar which points out why FloatLiteral must be placed before IntegerLiteral in the rule Literal. The reason is that FloatLiteral and IntegerLiteral could have common prefix that causes ambiguity. For example, if the IntegerLiteral comes before FloatLiteral, a text like '11.11' will be parsed into `[IntegerLiteral '.' IntegerLiteral]` which cannot match any grammar rule in our declaration.


### Let's do some test
Now that we have already defined all the core syntax in grammar.peg(you can checkout the full source code [here](https://github.com/ccbhj/peg_playground/blob/part_two/grammar.peg)).
Still we are gonna print the whole ast to check if there's any goes wrong:
```go
// peg_playground/parser_test.go 

// ...
func TestValue(t *testing.T) {
	var cases = []struct {
		Desc string
		Expr string
	}{
		{
			Desc: "NIL",
			Expr: "nil",
		},
		{
			Desc: "boolean",
			Expr: "#t",
		},
		{
			Desc: "decimal integer",
			Expr: "123456",
		},
		{
			Desc: "hex integer",
			Expr: "0x1f1",
		},
		{
			Desc: "short string",
			Expr: `"hello\n\x1f\u1234\U12345678"`,
		},
		{
			Desc: "long string",
			Expr: `"""foobar"""`,
		},
		{
			Desc: "identifier",
			Expr: `"""hello\n\x1f\u1234\U12345678"""`,
		},
		{
			Desc: "expression",
			Expr: `(HELLO world 123 "!!!")`,
		},
	}

	for _, c := range cases {
		println("[TEST] " + c.Desc)

		if err := PrintAST(c.Expr); err != nil {
			t.Fatal(err)
		}
		println("-----------------------------------------------------")
	}
}
```
For each rule I added a test case to see if they can be parsed. Now you can go this test by `go test -run=TestValue -v`. Here is the output that I got:
```
=== RUN   TestValue
[TEST] NIL
Script "nil"
 Value "nil"
  Literal "nil"
   NilLiteral "nil"
-----------------------------------------------------
[TEST] boolean
Script "#t"
 Value "#t"
  Literal "#t"
   BoolLiteral "#t"
-----------------------------------------------------
[TEST] decimal integer
Script "123456"
 Value "123456"
  Literal "123456"
   IntegerLiteral "123456"
    DecimalNumeral "123456"
-----------------------------------------------------
[TEST] hex integer
Script "0x1f1"
 Value "0x1f1"
  Literal "0x1f1"
   IntegerLiteral "0x1f1"
    HexNumeral "1f1"
     HexDigit "1"
     HexDigit "f"
     HexDigit "1"
-----------------------------------------------------
[TEST] short string
Script "\"hello\\n\\x1f\\u1234\\U12345678\""
 Value "\"hello\\n\\x1f\\u1234\\U12345678\""
  Literal "\"hello\\n\\x1f\\u1234\\U12345678\""
   StringLiteral "\"hello\\n\\x1f\\u1234\\U12345678\""
    StringChar "h"
    StringChar "e"
    StringChar "l"
    StringChar "l"
    StringChar "o"
    StringChar "\\n"
     Escape "\\n"
    StringChar "\\x1f"
     HexByte "\\x1f"
      HexDigit "1"
      HexDigit "f"
    StringChar "\\u1234"
     UChar "\\u1234"
      HexDigit "1"
      HexDigit "2"
      HexDigit "3"
      HexDigit "4"
    StringChar "\\U12345678"
     UChar "\\U12345678"
      HexDigit "1"
      HexDigit "2"
      HexDigit "3"
      HexDigit "4"
      HexDigit "5"
      HexDigit "6"
      HexDigit "7"
      HexDigit "8"
-----------------------------------------------------
[TEST] long string
Script "\"\"\"foobar\"\"\""
 Value "\"\"\"foobar\"\"\""
  Literal "\"\"\"foobar\"\"\""
   LongStringLiteral "\"\"\"foobar\"\"\""
    LongStringChar "f"
    LongStringChar "o"
    LongStringChar "o"
    LongStringChar "b"
    LongStringChar "a"
    LongStringChar "r"
-----------------------------------------------------
[TEST] identifier
Script "\"\"\"hello\\n\\x1f\\u1234\\U12345678\"\"\""
 Value "\"\"\"hello\\n\\x1f\\u1234\\U12345678\"\"\""
  Literal "\"\"\"hello\\n\\x1f\\u1234\\U12345678\"\"\""
   LongStringLiteral "\"\"\"hello\\n\\x1f\\u1234\\U12345678\"\"\""
    LongStringChar "h"
    LongStringChar "e"
    LongStringChar "l"
    LongStringChar "l"
    LongStringChar "o"
    LongStringChar "\\"
    LongStringChar "n"
    LongStringChar "\\"
    LongStringChar "x"
    LongStringChar "1"
    LongStringChar "f"
    LongStringChar "\\"
    LongStringChar "u"
    LongStringChar "1"
    LongStringChar "2"
    LongStringChar "3"
    LongStringChar "4"
    LongStringChar "\\"
    LongStringChar "U"
    LongStringChar "1"
    LongStringChar "2"
    LongStringChar "3"
    LongStringChar "4"
    LongStringChar "5"
    LongStringChar "6"
    LongStringChar "7"
    LongStringChar "8"
-----------------------------------------------------
[TEST] expression
Script "(HELLO world 123 \"!!!\")"
 Value "(HELLO world 123 \"!!!\")"
  Expression "(HELLO world 123 \"!!!\")"
   LPAR "("
   Operator "HELLO "
    Identifier "HELLO "
     IdentifierPrefix "H"
      Letter "H"
     IdentifierChar "E"
      LetterOrDigit "E"
     IdentifierChar "L"
      LetterOrDigit "L"
     IdentifierChar "L"
      LetterOrDigit "L"
     IdentifierChar "O"
      LetterOrDigit "O"
     Spacing " "
   Value "world "
    Identifier "world "
     IdentifierPrefix "w"
      Letter "w"
     IdentifierChar "o"
      LetterOrDigit "o"
     IdentifierChar "r"
      LetterOrDigit "r"
     IdentifierChar "l"
      LetterOrDigit "l"
     IdentifierChar "d"
      LetterOrDigit "d"
     Spacing " "
   Value "123 "
    Literal "123 "
     IntegerLiteral "123"
      DecimalNumeral "123"
     Spacing " "
   Value "\"!!!\""
    Literal "\"!!!\""
     StringLiteral "\"!!!\""
      StringChar "!"
      StringChar "!"
      StringChar "!"
   RPAR ")"
-----------------------------------------------------
--- PASS: TestValue (0.00s)
PASS
ok  	playground	0.651s
```
As you can see, there is no syntax error or parser error thrown, everything works just fine!

