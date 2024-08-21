---
author: ccbhj
pubDatetime: 2024-08-19T13:31:00Z
modDatetime: 2024-08-19T13:46:00Z
title: "Part III: Implement an expression interpreter for building DSL - Syntax tree evaluation"
slug: "part-three-implement-an-expression-interpreter-for-building-DSL"
featured: true
draft: false
tags:
  - gendsl
  - golang
  - DSL
description: "Introduce how to evaluate an S-Expression syntax tree"
---

## Table of contents


## Intro
In the last post, I have shown you how to implement the parser with the syntax of an S-Expression. In this post, I will demonstrate how we turn a syntax tree into any value we want which is so called evaluation. Meanwhile, I bet you might still wonder why I prefer S-Expression instead of the traditional C style syntax or something more like Lua, so I will also show you how powerful and elegant the idea behind the S-Expression and Lisp. 

## Let's build a tiny interpreter in a minute
As I have told you before in the first post of this series, the seed of developing gendsl was planted by the Youtube [video](https://www.youtube.com/watch?v=OyfBQmvr2Hc) presented by William Byrd. He had show me how easy it is to write a S-Expression interpreter and the way he achieved it was so elegant and beautiful. Now let me share it with you by writing an interpreter with scheme that interprets S-Expression. Before we start, let's forget everything about Golang or other language in C style and try to embrace the lisp style language in functional paradigm. Don't worry about the syntax since we have talked about S-Expression too much in the last few posts.

### Atom value
I gonna use the [Racket](https://racket-lang.org/) but feel free to use other lisp or scheme implementation like [mit-scheme](https://www.gnu.org/software/mit-scheme/), later you will get why we use lisp style language.
let's start by defining a simple function `expr-eval` which accepts an atom value, then return itself since an atom value is already a expression itself. 
```racket
#lang racket

(define expr-eval 
  (lambda (expr) 
   (cond 
     [(number? expr) expr]
     [(string? expr) expr]
     [else (error "invalid syntax")])))
```

The code here simply defines a function(indicated by the form `lambda`) called `expr-eval` that accepts an argument named `expr` and check the type of it with the builtin `cond` form that accepts a list of clauses whose first argument is a condition and the second argument is the action executed when first argument return true, if `expr` is a number or string, return it directly, otherwise just throw an error. Let's open the REPL and try it out:
```bash
> (expr-eval 123)
123
> (expr-eval "hello")
"hello"
```
It is simple and works fine so far. How about adding more form for operators like '+', '-' or '*'?

### Form
As we discuss before, when it comes to evaluation, for a S-Expression in the form of `(X Y Z)`, `X` will be treated as a procedure or function, and the rest of values will be arguments for it. So we can treat the first value in a form as the operator, and the rest of them as operands:
```racket
#lang racket

(define expr-eval 
  (lambda (expr) 
   (cond 
     [(number? expr) expr]
     [(string? expr) expr]
     [(list? expr) (let ([op (car expr)] [opr (cdr expr)])
                        (cond 
                          [(equal? op 'PLUS) (+ (first opr) (second opr))]
                          [else (error "unknown operator" op)]))]
     [else (error "invalid syntax")])))
```
We have added form parsing procedure in the line 8. Firstly we check if the `expr` is a list, if so, we assign(that is what the `let` is for) the head of the `expr`(by using `car`, which returns the head of a list) to `op` which is the operator, and the rest of the `expr` to `opr`(by using `cdr`, which returns the rest of the list other than its head as another list), then we check the `op`, if it equals to the symbol `PLUS`, we perform addition between the first and second operand and return the result.
Let's try it out:
```bash
> (expr-eval (PLUS 1 2))
PLUS: undefined;
 cannot reference an identifier before its definition
  in module: top-level
 [,bt for context]
```
Oops! Looks like we have an error here. The racket interpreter is telling us that it can't find the definition of PLUS, why? Because we are actually pass the result of `(PLUS 1 2)` instead of itself which is a list, therefore the racket interpreter will evaluate the form `(PLUS 1 2)` first and the use the result of it for the form `(expr-eval ...)`. You can try this:
```bash
(expr-eval (* 2 2))
4
```
It works! Though we haven't prepared anything for the operator '*'. That's because we pass the number 4 to our `expr-eval` instead of `(* 2 2)`.  So we must prevent the interpreter from evaluating the form `(PLUS 1 2)` we are using by adding a quote mark `'` in the front of it like `'(PLUS 1 2)` which can prevent racket interpreter from evaluating a form:
```bash
> (expr-eval '(PLUS 1 2))
3
```

Now it works! I have also demonstrated some usage example of the quote operator `'`:
```racket
> '(+ 1 2)
'(+ 1 2)  
> 'MUL
'MUL
> (symbol? 'MUL)
#t
```
As you can see, **when we quotes a form or a value, it will always return whatever it quotes, as well as a symbol like "PLUS" or even "+"**. That's why we have added the `'` in front of `PLUS` when checking the operator otherwise the racket interpreter will complain about the undefined symbol `PLUS`.
Now let's add another more complex test, how about a nested S-Expression:
```racket
> (expr-eval '(PLUS 1 (PLUS 2 3))) ; should return 6
+: contract violation
  expected: number?
  given: '(PLUS 2 3)
 [,bt for context]
```
Oops! Error thrown again. It seems that we have passed the nested expression `(PLUS 2 3)`(which is a list/form) to `+` but `+` can only accept number. Let's check what's wrong is going on around the `+`:
```racket
; ...
[(equal? op 'PLUS) (+ (first opr) (second opr))]
;                     ^^^^^^^^^^^^^^^^^^^^^^^
```
For the test case we used above, the `(first opr)` returns `1`, which is a number, but for `(second opr)`, it returns `'(PLUS 2 3)` which is a form however. So that's where it goes wrong: we forgot **that operands of an operator could be expressions too**. So, please bear it in mind that **we should never assume that a variable is an atom value, it could also be a form**. Now we need to turn each operand into value before we pass them to the operator function, but how can we do that? The answer is quite clear: use our `expr-eval` since that exactly what it does!
```racket
; ...
[(equal? op 'PLUS) (+ (expr-eval (first opr)) (expr-eval (second opr)))]
;                     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
```
The `(expr-eval '(PLUS 1 (PLUS 2 3)))` should be able to return 6 now. We can easily support more operands by adding more forms that check the operator and apply operands to a function. Here we add one more operator 'MUL' to support multiplication:
```racket
#lang racket

(define expr-eval
  (lambda (expr)
    (cond
      [(number? expr) expr]
      [(string? expr) expr]
      [(list? expr) (let ([op (car expr)] [opr (cdr expr)])
                      (cond
                        [(equal? op 'PLUS) (+ (expr-eval (first opr)) (expr-eval (second opr)))]
                        [(equal? op 'MUL) (* (expr-eval (first opr)) (expr-eval (second opr)))]
                        [else (error "unknown operator" op)]))]
      [else (error "invalid syntax")])))
```

### Symbol reference
Now let's add another feature for our tiny interpreter, how about refering variables/symbols by name? just like we use forms like `lambda`, `car`, `cdr` and so on. **Traditionally, we stores the mapping between names to variables in a table called "environment"**. For example, we can access environment variables stored in the kernel struct for an unix process, we can access global variables in a global table '_G' anywhere including the host language. 
Instead of using something like hash table to store the mapping from name and variable, we gonna use a function call `env` to map an identifier to a value.
```racket
#lang racket

(define expr-eval
  (lambda (expr env)
;               ^^^ new argument
    (cond
      [(number? expr) expr]
      [(string? expr) expr]
      [(list? expr) (let ([op (car expr)] [opr (cdr expr)])
                      (cond
                        [(equal? op 'PLUS) (+ (expr-eval (first opr) env) (expr-eval (second opr) env))]
;                                                                    ^^^                          ^^^   pass the `env` for nested expression
                        [(equal? op 'MUL) (* (expr-eval (first opr) env) (expr-eval (second opr) env))]
                        [else (error "unknown operator" op)]))]
      [(symbol? expr) (env expr)]
;     ^^^^^^^^^^^^^^^^^^^^^^^^^^^ transform symbol into values
      [else (error "invalid syntax")])))
```
We have add a new argument `env` which is a function that accepts a symbol and return the value for this symbol. When we found that `expr` is actually a symbol, we will use `env` to transform it into value. When evaluating a nested expression, we need to pass the same environment too. Let's see if it works as we exepected, the following expression should return 6:
```racket
> (expr-eval '(PLUS ONE (PLUS TWO 3)) (lambda (id) (case id
                                                      ['ONE 1]
                                                      ['TWO 2])))
6
```

### Implement lambda/function
Now let's implement something more tricky - lambda, or function which we used more common. Lambda is definitely a topic quite confusing since it involves name scope. And we gonna implement the [Lexical Scope](https://en.wikipedia.org/wiki/Scope_(computer_science)#Lexical_scope), which is used quite commonly in programming languages like C, even Pascal. Lexical scoping means a variable name binding always depend on its lexical context, which might be a function, or a code block like `{ }` we have in C. Since we are now implementing a lambda, when we refers a name defined both in a lambda and outside it lexical scope, it should always refers to what it binds in the lambda instead of the one outside the lambda. You will know when you see the code:
```racket
(define expr-eval
  (lambda (expr env)
    (cond
      [(number? expr) expr]
      [(string? expr) expr]
      [(list? expr) (let ([op (car expr)] [opr (cdr expr)])
                      (cond
                        [(equal? op 'PLUS) (+ (expr-eval (first opr) env) (expr-eval (second opr) env))]
                        [(equal? op 'MUL) (* (expr-eval (first opr) env) (expr-eval (second opr) env))]
                        [(equal? op 'FUNC) (let ([param (first opr)] [body (second opr)])
                                               (lambda (arg) (expr-eval body (lambda (id)
                                                                                  (if (equal? id param) 
                                                                                    arg
                                                                                    (env id))))))]
;                        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ lambda definition
                        [else ((expr-eval op env) (expr-eval (first opr) env))]))]
;                             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ try to interprete the `op` into a lambda
      [(symbol? expr) (env expr)]
      [else (error "invalid syntax")])))
```
At first glance, this is quite mind blowing. Here we define a operator `FUNC` that returns a lambda(or procedure if you will) whose accept a parameter and a body. What this lambda do is simple evaluating the body with a special nested environment which try to compares any name with the parameter, **if a name is the same as the parameter, we return the actual argument, otherwise we just pass it to the outter environment.** This is our key to implement lexcial scope.

Pay attention to the difference between a parameter and an argument which you must have been taught in your programming 101. **A parameter is the name of an argument to be passed to an function. Therefore, "parameter" is kind of a concept during compilation while "argument" is for the runtime.**

Finally, we always assume that the `op` is a lambda or procedure when it is not one of the pre-defined operator. This is actually how we evaluate a S-Expression which also provides us the way to apply arguments to a function. Now let's write a test function implements add-one sematic and apply an argument to it:
```
> (expr-eval '((FUNC v (PLUS v ONE)) TWO) (lambda (id) 
                                            (case id
                                                 ['ONE 1]
                                                 ['TWO 2]))) 
; 3
```
Inside the add-one function we can refer the parameter name `v` as well as the name `ONE` though it is defined outside the function.

As you can see, the magic behind all these is actually **recurssion**. since we are dealing with a tree made of lexical elements. As we discussed before, **the S-Expressions is actually a encoding of a syntax tree**. So when we are coding with S-Expression, we are actually encoding a syntax tree, which also makes it easy to manipulating the syntax tree itself.

This is all I want to share with you which is also I have learned in the presentation by William Byrd. I gotta say that this is completely mind-blowing as well as confusing and it really tooks me quite some time to understand the implementation of lambda. But trust me, bear it in your mind and try to implement it all by yourself then you can thanks me later. It can really provide you a whole new insight of programming. That's why I think that every programmer should try lisp and understand the beauty of it. 

## Implementation of an interpreter in golang
After you put the tiny interpreter above in mind, we can start to implement it in golang. We gonna implement every one of it with the syntax tree we have built in the last few post. You can check the complete implementation [here](https://github.com/ccbhj/gendsl/blob/main/grammar.peg).

### Evaluate entry
Our interpreter should have an entry function just like we did in our tiny interpreter. Firstly I gonna define a entry function that could evaluate every syntax node into value, just like the `expr-eval` we have above:
```go
type ParseContext struct {
    p *parser // peg parser
}

type EvalCtx struct {
	parent   *EvalCtx // EvalCtx from the outter scope, nil for top level scope
	env      *Env     // env for current scope
	UserData any      // UserData that is used across the entire script evaluation
}


var parserTab map[pegRule]func(*ParseContext, *EvalCtx, *node32) (any, error)

func init() {
    parserTab = map[pegRule]func(*ParseContext, *EvalCtx, *node32) (any, error){
        // ....
	}
}

func (c *ParseContext) parseNode(node *node32, evalCtx *EvalCtx) (any, error) {
	parser := parserTab[node.pegRule]
	if parser == nil {
		panic("parser for rule " + node.pegRule.String() + " not found")
	}
	return parser(c, evalCtx, node)
}
```
Since PEG will generate a node type enumeration for each syntax rule, we store the mapping of the node type and how to evaluate it(a function) in a table `parserTab`. The entry point `parseNode`, will find out how to eavluate a node and evaluate it into a value. In this way, we can use this function every time we want to evaluate a node without checking its node type.

### Parsing literal
As ususal, let's start with the simple one, the atom value or literal. Before we start to really parse some stuff, let's support literal type first.

#### Literal Type 
Since our DSL is embeded in golang which is a static typing language, import typing will makes it easy to port golang functions to our DSL. 
```go
// ValueType specify the type of an [Value]
type ValueType uint64

const (
	ValueTypeInt       = 1 << iota // Int
	ValueTypeUInt                  // Uint
	ValueTypeString                // String
	ValueTypeBool                  // Bool
	ValueTypeFloat                 // Float
	ValueTypeProcedure             // Procedure
	ValueTypeUserData              // UserData
	ValueTypeNil                   // Nil
)

type Value interface {
	// Type return the ValueType of a Value.
	Type() ValueType
	// Unwrap convert Value to go value, so that it can be used with type conversion syntax.
	Unwrap() any
	_value()
}

type Int int64

var _ Value = Int(0)

func (Int) _value()         {}
func (Int) Type() ValueType { return ValueTypeInt }
func (i Int) Unwrap() any   { return int64(i) }

// ... more types
```
Inspired by the [gopher-lua](github.com/yuin/gopher-lua) which is an implement of lua VM in golang, we use an interface `Value` to unify all the types we have. You might have noticed we have something different from those basic type we have in golang, the `Procedure`, `UserData` and `Nil`. These are special types for the convience of import golang function. The `Procedure`, which is actually `func(evalCtx *EvalCtx, args []Expr, options map[string]Value) (Value, error)`, is actually a protocol for injecting golang functions in our DSL, we will get to this later. `UserData` are for injecting complex data type, also inspired by gopher-lua.

#### Nil
Now that we have typing, we can start to really parse something. Let's start from the simple one - the `Nil`.
```go
func parseNilLiteral(c *ParseContext, _ *EvalCtx, node *node32) (any, error) {
	return Nil{}, nil
}

func init() {
	parserTab = map[pegRule]func(*ParseContext, *EvalCtx, *node32) (any, error){
        // ... 
		ruleNilLiteral:        parseNilLiteral,
	}
}
```
Parsing `Nil` rule is quite simple. There is no string processing or any validation because it has already token care by the PEG parser. All we have to do is just return the wrapped type for `nil`. And finally, we need to register this function in `parserTab` so that we can use the `parseNode` to parse the `ruleNil`.

#### Float
Let's see another example for float literal which take the advantage of the golang standard library:
```
func parseFloatLiteral(c *ParseContext, _ *EvalCtx, node *node32) (any, error) {
	text := c.nodeText(node)
	v, err := strconv.ParseFloat(text, 64)
	if err != nil {
		return nil, evalErrorf(c, node, "invalid float literal %q: %s", text, err)
	}
	return Float(v), nil
}

func (c *ParseContext) nodeText(n *node32) string {
	return strings.TrimSpace(string(c.p.buffer[n.begin:n.end]))
}

// don't forget to register the parseFloatLiteral in the parserTab
```
The `parseFloatLiteral` extracts text from the syntax node and use the `strconv.ParseFloat` to transform this text into float64. That's why I support so much syntax for float number - if `strconv.ParseFloat` can support so much syntax, why don't we take advantage of it?

### Parsing identifiers
Parsing an identifier is quite simple. We just read the text in the syntax node and look it up in the `env` we talked about before:
```go

func parseIdentifier(c *ParseContext, evalCtx *EvalCtx, node *node32) (any, error) {
	id := c.nodeText(node)
	v, ok := evalCtx.Lookup(id)
	if !ok {
		return nil, newUnboundedIdentifierError(c, node, id)
	}
	return v, nil
}
```
The tricky part is the `evalCtx.Lookup()` which implements the dynamic scope. Well not that tricky, since we have already know that magic behind this is recurssion:
```go
type Env struct {
	m map[string]Value
}

func (e *Env) Lookup(id string) (v Value, found bool) {
	v, ok := e.m[id]
	return v, ok
}

type EvalCtx struct {
	parent   *EvalCtx // EvalCtx from the outter scope, nil for top level scope
	env      *Env     // env for current scope
    // ...
}

func (e *EvalCtx) Lookup(id string) (Value, bool) {
    if e == nil || e.env == nil {
		return nil, false
	}
	v, ok := e.env.Lookup(id)
	if ok {
		return v, ok
	}

	if e.parent == nil {
		return nil, false
	}

	return e.parent.Lookup(id)
}
```
To implement lexcial scoping, we could just look up the identifier in the current scope's env, if we can't find it in the current scope, we could just look it up in the parent env util it reaches the top level scope which is the one we passed from golang.

### Parsing forms
Here comes the most complex part, the implementation of expression in form. It should be able to transform the operator into a `Procedure` which is actually a golang function and collect operands for it.
```go

type (
	EvalOpt struct {
		Env *Env // Environment that is only expose to this expression, but the identifier from outer scope can be still accessed.
	}

	Expr struct {
		node    *node32
		evalCtx *EvalCtx
		pc      *ParseContext
	}

	Procedure func(evalCtx *EvalCtx, args []Expr, options map[string]Value) (Value, error)
)

func parseExpression(c *ParseContext, evalCtx *EvalCtx, node *node32) (any, error) {
	cur := node.up
	cur = cur.next // ignore the LPAR

    // found out the procedure of this operator
	v, err := c.parseNode(cur, evalCtx)
	if err != nil {
		return nil, err
	}
	op, ok := v.(Procedure)
	if !ok {
		return nil, evalErrorf(c, node, "<%s> is not an procedure", v)
	}

	operands := make([]Expr, 0)
	cur = cur.next
	for ; cur != nil; cur = cur.next {
		switch cur.pegRule {
		case ruleLPAR, ruleRPAR:
			continue
		case ruleValue:
			node := cur.up
			operands = append(operands, Expr{node: node, evalCtx: evalCtx, pc: c})
		}
	}
	return op(evalCtx, operands, options)
}
```
What the `parseExpression` function do is, try to find out which procedure is mapping to this operator with the almighty `parseNode`, then we **convert the rest of the nodes into `Expr` instead of evaluating them**, and finally we execute this procedure. Why don't we evaluate those arguments then feed them to the procedure? That's the most interesting part where we allow users to control the order of evaluation. Within the procedure, users can control the evaluation by calling `Expr.Eval()`
```go
func (e Expr) Eval() (Value, error) {
	return e.EvalWithOptions(EvalOpt{})
}

func (e Expr) EvalWithOptions(opt EvalOpt) (Value, error) {
	var (
		evalCtx = e.evalCtx
		node    = e.node
		pc      = e.pc
	)

	env := opt.Env
	if env != nil {
		evalCtx = NewEvalCtx(evalCtx, evalCtx.UserData, env)
	}
	v, err := pc.parseNode(node, evalCtx)
	if err != nil {
		return nil, err
	}
	return v.(Value), nil
}
```
The `Expr.Eval` does the actual evalution that returns the value. This could allow a procedure behave more like a macro instead of function since the evaluation of a syntax tree is controlled by us. With this ability, we can easily create procedure with "if" semantic like this:
```go
IF := func(_ *gendsl.EvalCtx, args []gendsl.Expr, options map[string]gendsl.Value) (gendsl.Value, error) {
    cond, err := args[0].Eval()
    if err != nil {
        return nil, err
    }
    if cond.Type() != gendsl.ValueTypeNil { // not-nil is considered as true
        return args[1].Eval()
    } else {
        return args[2].Eval()
    }
}
// (IF 1 "foo" "bar") ; "foo"
// (IF nil "foo" "bar") ; "bar"
```
And if there is an `Env` provided, we will derive a new `EvalCtx` from so that this `Expr` can read the variables from this `Env` along and the outter scope. Besides, we can also put variables into the inner `Env` without affecting the outter `Env`. This allows us to write procedure act like the `let` form in lisp:
```go
LET := func(_ *gendsl.EvalCtx, args []gendsl.Expr, options map[string]gendsl.Value) (gendsl.Value, error) {
    nameExpr := args[0]
    if nameExpr.Type() != gendsl.ExprTypeIdentifier {
        return nil, errors.New("expecting an identifier")
    }

    name := strings.TrimSpace(nameExpr.Text())

    val, err := args[1].Eval()
    if err != nil {
        return nil, err
    }

    return args[2].EvalWithEnv(ectx.Env().WithValue(name, val))
}

// (LET foo "bar" (PRINTLN foo)) ; "bar"
```

### Show cases
I gonna show you how to export a DSL that allows you to define a JSON. You can find the full source code [here](https://github.com/ccbhj/gendsl/blob/main/examples/example_json_test.go). Let's see how it looks firstly:
```racket
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
As you can see, it looks much more like lisp, but it can really produce a JSON string, besides you can write if-else clauses in it to control what to put in the json body. Here is how is done:
```go
import "github.com/ccbhj/gendsl"

func EvalJSON(script string) ([]byte, error) {
    var env = gendsl.NewEnv().WithProcedure("json", gendsl.Procedure{
        Eval: _json,
    })
	jsonResult, err := gendsl.EvalExpr(script, env)
	if err != nil {
		return nil, err
	}

	return jsonResult.(*gendsl.UserData).V.([]byte), nil
}

type KV struct {
	K string
	V any
}

// only in json env would these operators can be accessed
var jsonOps = gendsl.NewEnv().
	WithProcedure("if", gendsl.Procedure{Eval: _if}).
	WithProcedure("array", gendsl.Procedure{Eval: _array}).
	WithProcedure("kv", gendsl.Procedure{Eval: _kv}).
	WithProcedure("dict", gendsl.Procedure{Eval: _dict})


func _json(evalCtx *gendsl.EvalCtx, args []gendsl.Expr, _ map[string]gendsl.Value) (gendsl.Value, error) {
	ret := make(map[string]any)
	for _, arg := range args {
		v, err := arg.EvalWithEnv(jsonOps) // inject some operators in this context
		if err != nil {
			return nil, err
		}
		kv, ok := v.Unwrap().(KV)
		if !ok {
			panic("expecting a kv")
		}
		ret[kv.K] = kv.V
	}

	b, err := json.Marshal(ret)
	if err != nil {
		return nil, err
	}

	return &gendsl.UserData{V: b}, nil
}
```
We define the main entry for parsing our tiny language. It basically just evaluates it with a procedure `_json` and extract json body. What the `_json` do is just evaluating every operands with some procedures and convert them into a key value pair which got saved in a map. In the end, we just serialize the map into json string and return it.

The most import part is the operands used in `json` form. You may have noticed that we can't access these procedures outside the `json` form, which somehow behaves like a package or module. For the procedure `if`, we had introduced to you before so I gonna skip it here. Let's see how the `array`, `kv` and `dict` works.

`array` form just evaluates every operands and collect them into an array:
```go
func _array(_ *gendsl.EvalCtx, args []gendsl.Expr, _ map[string]gendsl.Value) (gendsl.Value, error) {
	val := make([]any, 0, 1)
	for _, arg := range args {
		v, err := arg.Eval()
		if err != nil {
			return nil, err
		}
		val = append(val, v.Unwrap())
	}

	// return &gendsl.UserData{} to wrap any data into gendsl.Value
	return &gendsl.UserData{V: val}, nil
}
```

`kv` form just evaluates the first operand as key and the second as value:
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
	// return &gendsl.UserData{} to wrap any data into gendsl.Value
	return &gendsl.UserData{V: KV{K: string(key.(gendsl.String)), V: val.Unwrap()}}, nil
}
```

`dict` form just assume every operand is a key value pair and collect them into a map:
```go
func _dict(_ *gendsl.EvalCtx, args []gendsl.Expr, _ map[string]gendsl.Value) (gendsl.Value, error) {
	val := make(map[string]any)
	for _, arg := range args {
		v, err := arg.Eval()
		if err != nil {
			return nil, err
		}
		kv, ok := v.Unwrap().(KV)
		if !ok {
			return nil, errors.New("expecting a kv")
		}
		val[kv.K] = kv.V
	}

	return &gendsl.UserData{V: val}, nil
}
```
These procedures are extremely short and clean. There is no nasty grammar work in parser but just the simplicity and elegance of recurssion. Although executing everything during runtime might cause slowness, and you may have to wait until it crashes to find syntax errors in your script, as I said, we are not inventing a new language for system programming nor high concurrency but just finding a way to make DSL easier and for better user experience.

In these show cases, I have shown you how to define an `if` and `let` expression without access any parser nor changing the grammar rule. I think this is the most significant feature in gendsl and it has satisfied all my need that I described in the [Intro](https://ccbhj.com/posts/intro-implement-an-expression-interpreter-for-building-DSL/): 
- My own lispy syntax powered by S-Expression.
- I can access go value in the DSL by reading and writing the env passed by golang.
- Data type, all values have their own type.
- Extension requires no modification of the grammar, you can control the AST in a procedure during runtime.

## The end
What I have introduced to you above, is the core of my project [gendsl](https://github.com/ccbhj/gendsl) which is greatly inspired by the lisp. The lisp, as well as the presentation by William Byrd, really give us a different view on how our program got evaluated, which often got ignored by us. **Most of programmers today are caught in a "Con Air" that called "business". We fly so high and fast, and got satisfied by telling ourself 'Well, it works' but somehow we ignore the beauty behind all of those computing and evaluting**. But when we slow it down and have a peek at what's behind and what brings us **Inverse Vandalism**(make things just because you can that Alan Kay talk about in the [The early history of Smalltalk](https://dl.acm.org/doi/10.1145/155360.155364)), things will be clear and your softwares could just be elegant and beautiful as you know how thing become elegance and beauty.
