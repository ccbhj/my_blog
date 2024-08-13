---
author: ccbhj
pubDatetime: 2024-08-07T00:00:00Z
modDatetime: 2024-08-07T00:00:00Z
title: "Part I: Implement an expression interpreter for building DSL - Introduce the PEG parser"
slug: "part-one-implement-an-expression-interpreter-for-building-DSL"
featured: true
draft: false
tags:
  - gendsl
  - golang
  - DSL
description: "Introduce the basic steps to implement an interpreter and the powerful PEG parser generative tool."
---

## Table of contents

In the last post I have introduced you the WHY and HOW I start the this project and WHAT the DSL looks like in the end. Starting from this post I will share the implementation of the whole project with you. 

Generally, when we implement a language, the first thing comes in our mind is lexer and then parser. So in this post I will introduce to you how I implement my DSL with specified detail but less conception so that it won't be too confused I hope.

## What's lexer?
In general, lexer is used for [Lexical Analysis](https://en.wikipedia.org/wiki/Lexical_analysis) or tokenizer if you will. Let's take the clause _"We will rock you!"_ (the famous rock and roll music from Queen) as an example. After we tokenize it following the rule of English, it will output a list like this `[Subject("We"), Auxiliary("will"), Verb("rock"), Object("you"), Punctuation("!")]`. So **a lexer is mostly used for classify texts into certain types by its lexical meaning.** This is significant for us since the a syntax is actually consist of lexical elements instead of characters or words. 

Usually we implement a lexer with some code generator that can parse [Regular Express](https://en.wikipedia.org/wiki/Regular_expression) like [Ragel](https://en.wikipedia.org/wiki/Ragel), [nex](https://github.com/blynn/nex) and so on. But I think you will be surprised how easy it is to implement a lexer after checking [Lexical Scanning in Go](
https://www.youtube.com/watch?v=HxaD_trXwRE) from Rob Pike. He had introduced an interesting pattern to implement a finite-state machine which I think is the core of an lexer.

## How about the parser?
So how about a parser? What is it used for? Basically, **a parser is used to identify a list of lexical elements with the specified pattern which we also called it grammar.** In the example _"We will rock you!"_ we introduced in above, it produces a sequence of `[Subject("We"), Auxiliary("will"), Verb("rock"), Object("you"), Punctuation("!")]` which matches the pattern of 'Future tense' grammar in English. So this is exactly what a parser do and is so called '_syntax analysis_'. 

Let's take another example in a more computer way, how about an expression like `1 + 2 * 3` ? It is obvious that they will be translated into `[Number(1), Operator(+), Number(2), Operator(*), Number(3)]` by the lexer, but what this sequence will be translated into by a parser with a common mathematical expression grammar? In general, a lexical elements sequence will got translated into an _Abstract Syntax Tree_(or AST in short) by parser like this: 
```
      *
     / \
    +   3
   /  \
  1    2
```
With an abstract syntax tree we can now analyze the syntax in the correct order according to the rule of our grammar. 
## Let's implement a parser
Now we are gonna implement an parser on our own. Well not entirely on our own, we still need some tool to help our to generate code for our parser since it is hard to implement it correctly and a hand-writing parser might be difficult to maintain, even if you do, the performance might be poor.

Luckily, there are many mature parser generate tool that helps us to generate golang code with a grammar definition file. The first choice came in my mind is [go-yacc](https://pkg.go.dev/golang.org/x/tools/cmd/goyacc) which is the official tools to generate go code for parser. I used to use it to write an SQL analyzer and I know **it requires an external lexer** so I gave it up. "Why not try something new?" I think, so there we go, with the amazing tool [peg](https://github.com/pointlander/peg) I was able to implement both of the lexer and parser in one single grammar file, one single interface. Let's take a closer look at the peg.

### A closer look at PEG
PEG actually represents [Parsing Expression Grammar](https://en.wikipedia.org/wiki/Parsing_expression_grammar) introduced by Bryan Ford in 2004, which is an alternative to the traditional [Context Free Grammar](https://en.wikipedia.org/wiki/Context-free_grammar) used for describing and expressing programming language syntax and protocol.

For the last decades, we have been using parser code generate tool like [yacc](https://en.wikipedia.org/wiki/Yacc), [bison](https://en.wikipedia.org/wiki/GNU_Bison) that supports CFG to generate parser code, and if you have used them before you might find it difficult to avoid ambiguity and integrate them with the lexer or the regular expression. In fact, the syntax of a programming language is not just patterns of lexical elements but also the rules of those lexical elements which somehow the CFG is missing so when we use tools like yacc **we will have to implement lexer by our self**. Further more, to avoid ambiguity(like the precedence between plus and multiply, check [this](https://www.gnu.org/software/bison/manual/html_node/Precedence.html) out) in CFG **we have to define the precedence for each operator**. All of these crucial fact makes it unnecessarily difficult to develop a parser.

But thanks to Bryan Ford, now we have another choice, the PEG that allows us to define the lexical and syntax rule all in one single file with a tight DSL and resolve ambiguity in an elegant and simple way. Let me show you how easy it can be done with peg. 

#### Example and code come first
I gonna take examples from my gendsl library which implements a lisp-like syntax(you can check it out [here](https://github.com/ccbhj/gendsl/blob/main/grammar.peg)). Here is a simple snippet that can parse hex and decimal number literals: 
```
package playground

type parser Peg {
}

Script          <- Value EOF

EOF             <- !.

Value           <- IntegerLiteral
 
IntegerLiteral  <- [+\-]? ('0' ('x' / 'X') HexNumeral 
                           / DecimalNumeral ) [uU]?

HexNumeral      <- HexDigit ([_]* HexDigit)* / '0'

HexDigit        <- [0-9] / [A-F] / [a-f]

DecimalNumeral  <- [1-9] ([_]* [0-9])* / '0'     

# ...                      
```
The first line `package gendsl` is package declaration which decides which package the generated golang file should be. The following type declaration `type parser Peg {}` used to define the parser type, which we will use it later in evaluation but you can ignore it for now.

After the type declaration we can start to define your syntax rule till the end. This is different from the workflow I used to work with with yacc when I have to define a union type and a lot of token types before I can actually define my grammar, which could be really confusing. Anyway, let's take a look at the grammar definition. If you have worked with CFG before you might find the definition DSL syntax quite familiar. The right hand side of the '<-' refers to the pattern of lexical elements, which could be some other patterns or character sequence, and the left hand side is the name of the pattern. Pretty easy, right? 

#### The very first rule
Let's pay attention to the first pattern rule here since **the first rule is always to entry point of the parser.** The entry point `Script` here is consist of two parts, one is a rule refers to `Value` which is consist of a sequence of specified characters(we will get back to this later), the other one `EOF` is kind of interesting. Let's jump to the next rule to find the pattern of `EOF`. As you can see, _EOF_ is consist of `!.`. What does `!.` mean? The `!`actually means `NOT`, and `.` means any character, so **`!.` means `NOTHING AT ALL` or `End Of File` if you will**. As a result whenever the parser find there is no character to read, it will stop here and treat it as an dummy rule call `EOF` which might produces the rule `Script`.

#### More about the rule syntax 
So much like the regular expression(RE), the syntax of PEG is simple: 

- `.` stands for any character.
- character set like `[a-z]` set is also supported.
- `X` matches one character or pattern where X could be a character or rule name.
- `X?` matches zero or one character or pattern, while `X*` matches zero or more and 'X+' matches one or more.
- `X \ Y` matches X or Y where X and Y are character or rule name. 

Take the rule of `DecimalNumeral` as an example. `[1-9]` means the start of an `DecimalNumeral` must be one of 1-9, `([_]* [0-9])*` means starring from the second position, all character, if there is any, must be a digit(0-9) that has might have no `'_'` or more that one `'_'` as its prefix so it could match string like `"10_2_3"`. Otherwise, indicated by the operator `\`, it could just be one single character `'0'` which means 0 obviously .

#### Resolving ambiguity
I want to spend more time to explain the or operator `\`, since it is quite important as the solution to the ambiguity. **The PEG will always try to match the first pattern which is considered as earliest-match-first**. For example, a string `"ab"` will never be able to match the grammar `G <- 'a' / 'a' 'b'`, since the first character `'a'` will be reduced to `G` but the `'b'` left cannot match anything. By the way, CFG doesn't allow such a rule and will throw the reduce/shift conflict error.

There is no much syntax left, you can explore them yourself in the [pointlander/peg README](https://github.com/pointlander/peg) or [peg doc](https://www.piumarta.com/software/peg/peg.1.html). 

#### Let's have a try
Now that we already have a simple syntax rule prepared above, though it is not the whole grammar for the gendsl project but it can still parse some numbers. Anyway let's generate some code and see if it works as we expect.
##### Preparation
First we have to install the peg binary tool for code generate following this [guide](https://github.com/pointlander/peg?tab=readme-ov-file#installing), then we gonna setup our workspace directory for playing:
```
> mkdir peg_playground && peg_playground 
> go mod init peg_playground 
> touch grammar.peg
```
Paste the grammar we have before into the `peg_playground/grammar.peg`, now we should be able to genreate the code using the generate tool but why not make a Makefile in `peg_playground/makefile`
```make
GO := go

.SUFFIXES: .peg .go

grammar.go: grammar.peg
	peg -switch -inline -strict -output ./$@ $<

all: grammar.go

clean:
	rm grammar.go 

```

##### Generate and test the parser 
Now that we have everything ready, let's generate the code for parser:
```sh
make grammar.go
```
After running the command, you should see a generated grammar.go in the workspace directory. Let's write a function to parse a string and access our parser:
```go
// peg_playground/parser.go
package playground

func PrintAST(script string) error {
	parser := &parser{
		Buffer: script,
		Pretty: true,
	}

	if err := parser.Init(); err != nil {
		return err
	}
	if err := parser.Parse(); err != nil {
		return err
	}

	parser.PrintSyntaxTree()
	return nil
}
```
The snippet here is pretty simple, it initializes the parser, parses the script we pass to it and print the syntax tree in final. Let's write an unit test to see if it works:
```go
// peg_playground/parser_test.go
package playground

import (
	"testing"
)

func TestPrintTree(t *testing.T) {
	if err := PrintAST(`0x123`); err != nil {
		t.Fatal(err)
	}
	t.Log("-----------------------------------------------------")

	if err := PrintAST(`10_2_3`); err != nil {
		t.Fatal(err)
	}
	t.Log("-----------------------------------------------------")
}
```
The test function `TestPrintTree` calls the `PrintAST` and check the error. Let's run it now and see what it gonna print:
```
go test . -v
```
Now we should see the whole syntax tree in the output:
```
=== RUN   TestPrintTree
Script "0x123"
 Value "0x123"
  IntegerLiteral "0x123"
   HexNumeral "123"
    HexDigit "1"
    HexDigit "2"
    HexDigit "3"
    parser_test.go:11: -----------------------------------------------------
Script "10_2_3"
 Value "10_2_3"
  IntegerLiteral "10_2_3"
   DecimalNumeral "10_2_3"
    parser_test.go:16: -----------------------------------------------------
--- PASS: TestPrintTree (0.00s)
PASS
ok      playground      0.649s
```
It looks great, right? Everything works as we expected. No syntax error thrown and it prints every rule matched and the string it matches in a format of tree, which could be really useful when debugging.

## Wrap it up
In this post, I have introduced you the two basic but significant parts of interpreter programming language:

+ Lexer, for lexical analysis that transforms a string into a sequence of lexical elements.
+ Parser, for syntax analysis that identify the the pattern (so called grammar) in the lexical elements and produces a syntax tree.

And then I introduce the PEG for parser code generating, and address its advantages comparing the traditional CFG:

+ Lexer rule integrated, no standalone lexer need to be implemented.
+ Simple, regular expression like syntax to start up fast.
+ No ambiguity, no reduce/shift conflict, always earlier-match-first.
Finally I have a litte demonstration of how to generate parser with PEG, which is the basis of our interpreter. In next post, I will walk you through the gendsl grammar in detail.

Thank you for checking this post, hope you enjoy it.
