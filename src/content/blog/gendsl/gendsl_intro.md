---
author: ccbhj
pubDatetime: 2024-07-20T15:22:00Z
modDatetime: 2024-07-22T09:12:47.400Z
title: "Intro: Implement an expression interpreter for building DSL"
slug: "intro-implement-an-expression-interpreter-for-building-DSL"
featured: true
draft: false
tags:
  - gendsl
  - golang
  - DSL
description:
  Intro to my first open source project, gendsl, an expression interpreter in lisp style written in golang for building your own DSL in minutes.
---

## Table of contents

## Intro
The other day I ran into a meetup recording [video](https://www.youtube.com/watch?v=OyfBQmvr2Hc) in Youtube presented by William Byrd(a funny and very wise guy) talking about how to write an interpreter in scheme that evaluates scheme itself, which got me fascinated immediately by the simplicity, elegance and power of the meta programing language like common-lisp, scheme and other language in lisp family(you may heard of the parenthesis joke about lisp). I think this video had provided me exactly the right way to introduce a DSL for my rule engine that I have been working on for the last few weeks. So eventually I had came out a DSL framework that enables you to write you DSL in a minutes without dealing with any lexer or parser - [a general DSL](https://github.com/ccbhj/gendsl)

I will make a series of blog to share my work and some thoughts during my development. But first, let me show you what I think about the beauty of the idea of DSL(Domain Specified Language) and the story about how I start this project.


## Why DSL?
What do we need DSL? The answer is quite simple: **to have better interaction between our program and users**.
Think about it!  Most of our programs or apps accepts some key value pairs as an input. For example, a command line tool may accept users input from arguments(e.g. "--key value"), or environment variable(e.g. "KEY=VALUE ./cli"). Even for a http server, we may accept url query(e.g. "http://127.0.0.1:80?key=value"), headers(e.g. "Content-Type: application/json"), or body in json(e.g '{"key": "value"}'). But when a program's behavior depend heavily on user input, key value pairs will become insufficient. Take the famous awk for example, how could you tell awk how to query/filter/extract/format strings with key value pair? Even if you could, the user experience will be a disaster since users will have to fill a lot of key to describe his/her need. 

However, with a proper designed DSL as the input of your app, you can have:
1. **Better user experience**. Since the DSL is domain specified, it can describe what the user want in a simple and brief way so that users won't have to input too much.
2. **Decouple with protocol and types**. Since a DSL expression is just nothing more than just a simple string, you can easily pass it in the command line arguments, environment variable, http query, http body or even socket. Besides, you can read it in any language(since most language support string type) and that is the beauty of SQL which adapts so many language client and different databases. 
3. **Readability**. Not only it makes it easy for users to describe their need easily, but also frees you from all those checking and validation of users input. The interpreter will do that for you since the grammar is always of certainty.

## What I need in my DSL?
    
Now that I have a reason to import DSL in my rule engine, I should consider what it gonna look like.
Months ago I happened to be asked by my leader to write an expression engine that can evaluate expressions into elasticsearch query or into a golang value, and I solved it by introducing the golang's standard library [go/parser](https://pkg.go.dev/go/parser) to turn golang expressions into an AST(Abstract Syntax Tree) and parse it into different values like elasticsearch query, a simple boolean value or something else. With a few lines of code like this you can parse a golang expression in anyway you want:
```go
package main

import (
	"fmt"
	"go/ast"
	"go/parser"
	"go/token"
)

func main() {
	expr := "true && true"
	root, err := parser.ParseExpr(expr)
	if err != nil {
		panic(err)
	}
	fmt.Println(parseNode(root).(bool)) // => true
}

func parseNode(node ast.Node) any {
	switch v := node.(type) {
	case *ast.BinaryExpr:
		switch v.Op {
		case token.LAND:
			return parseNode(v.X).(bool) && parseNode(v.Y).(bool)
			// ...
		}
	case *ast.Ident:
		switch v.Name {
		case "true":
			return true
		case "false":
			return false
			// ...
		}
		// ...
	}
	panic("unsupported node")
}
```
I gotta say that the decision of using golang's standard library was a good choice then because:
   1. Only 5 days I was given to finish this job and with the standard library I can have a stable, well-tested compiler without using any lexer or parser.
   2. The golang expression syntax is pretty enough for me.
   3. My colleagues are good at golang so the syntax will be easy to learn and use for them. 

But there are also some downsides that I have to deal with it when writing the DSL for my rule engine:

   1. **You can't customize your own syntax**. You can only have whatever the golang has, not your own keyword, not your own data types. 
   2. **The readability is terrible** as the expression becomes complicated.
   3. **Parsing and debugging a AST is difficult** to understand for my teammates and sometimes even for me(although I was the one who wrote it).

So I decide to write my own interpreter and I want it to have: 

   1. **My own syntax**, since I am looking for a DSL for a specified domain problem instead of Turing complete programming language.
   2. **Accessiblity** to values including functions from the host language. I should be able to access value of the host language in the DSL.
   3. **Simplicity and readability.** I want everyone can pick it up and extends it in five minutes.

After checking out the presentation by William Byrd and some digging into the [Racket](https://en.wikipedia.org/wiki/Racket_(programming_language))(another dialect of Lisp), I had already been clear about my tiny DSL. It should be:

1. **It should have the [S-Expression](https://en.wikipedia.org/wiki/S-expression) syntax.** **A S-Expression is either a atom value of a list of atom values.** This could make the syntax easy to learn and simple to parse.
2. **It should allow me to access values and even functions** from the host language, which is golang for my case.
3. **It should have its own data types** so that accessing values or functions in host language in the DSL or writing extension for our DSL won't be confusing.
4. **It should be easy to extend the DSL**, or in another word,  to control the evaluation of an expression. It was confusing for my colleagues when I am explaining how to implement short circuit for a boolean expression.

At this time, I had realized that **I can build more of a DSL for my rule engine but a general and flexible framework to build DSL with the powerful and beautiful S-Expression**.

## So what does it look like finally?
At the first glance, it looks just like lisp, here is a simple example that can produce a json:
```lisp
(json
 (if (later-than $NOW 2012)                                  ; condition
     (kv "language" (array "c" "c++" "javascript" "elixir")) ; then
     (kv "language" (array "c" "c++" "javascript")))         ; else
 (kv "typing"
     (dict
      (kv "c" "static")
      (kv "c++" "static")
      (kv "javascript" "dynamic")))
 )
```
As you can see, the syntax is quite simple. Everything is basically consist of S-Expression, which is **either an atomic value like integer/string/boolean/... or an expression in the format of '(X Y Z...)' where 'X' is considered as a function that produces an S-Expression and 'Y', 'Z', ... are considered as the arguments of function 'X'.** All those functions like 'kv', 'dict', 'array' and even 'if' are actually implemented in golang. Take 'kv' as an example, the implementation is quite simple:
```go
// _kv evaluate a key and its value then assemble them to a KV struct
func _kv(_ *gendsl.EvalCtx, args []gendsl.Expr, _ map[string]gendsl.Value) (gendsl.Value, error) {
	key, err := args[0].Eval()
	if err != nil {
		return nil, err
	}
	if key.Type() != gendsl.ValueTypeString {
		return nil, errors.Errorf("key is not string, but %+v", key)
	}
	val, err := args[1].Eval()
	if err != nil {
		return nil, err
	}
	return &gendsl.UserData{V: KV{K: string(key.(gendsl.String)), V: val.Unwrap()}}, nil
}
``` 
The procedure 'kv' takes two arguments which are actually expressions, evaluates them by `args[n].Eval()` into values, then constructs a KV struct and return it. With the powerful Eval method, we can even implement 'if' behavior that act more like a macro instead of just simple function:
```go
// _if check the condition of the first argument,
// if condition returns not nil, evaluate the second argument, otherwise the third argument
func _if(_ *gendsl.EvalCtx, args []gendsl.Expr, _ map[string]gendsl.Value) (gendsl.Value, error) {
	condEnv := gendsl.NewEnv().
		WithProcedure("later-than", gendsl.Procedure{Eval: gendsl.CheckNArgs("2", _laterThan)}).
		WithInt("$NOW", _year())

	cond, err := args[0].EvalWithEnv(condEnv)
	if err != nil {
		return nil, err
	}
	if cond.Type() == gendsl.ValueTypeBool && cond.Unwrap().(bool) == true {
		return args[1].Eval()
	}

	return args[2].Eval()
}
```
You may have noticed, **if the condition(first argument) returns true, only the second expression will be evaluated but not the third expression**! This can never be done with languages like golang that is without macro because we are actually manipulate the AST. So amazing, right?

Finally, with a few lines of code to implement functions like 'kv', 'dict', 'array', and 'if', the script above will produce a JSON output like this(full implementation is [here](https://github.com/ccbhj/gendsl/blob/main/examples/example_json_test.go)):
```json
{
   "language": ["c", "c++", "javascript", "elixir"],
   "typing": {
      "c": "static",
      "c++": "static",
      "javascript"": "dynamic"
   }
}
```

## To be continue...
As you can see above, the DSL engine I came up with is:
- Highly customizable.
- Easy to learn and extend.
- Typing that is accessible.
- Full control of the syntax tree.
In the following articles I will share with you about how I implement it from scratch which is really fun and great journey for me. 
Hope you enjoy this post ❤️
