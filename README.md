# Dargon

A simple programming language vaguely inspired the ML family of languages.  
This is a work in progress! Made for fun to better understand type systems.  

Resources used:

* [Type Inference and Unification, CS3110 Cornell](http://www.cs.cornell.edu/Courses/cs3110/2009fa/lectures/lec26a.htm)
* [Algorithm W Step by Step, Martin Grabmuller](https://github.com/wh5a/Algorithm-W-Step-By-Step)
* [Write You a Haskell, Stephen Diehl](http://dev.stephendiehl.com/fun/)
* [Type and Programming Languages, Benjamin C. Pierce](https://www.cis.upenn.edu/~bcpierce/tapl/)

## Some examples

```ml
> let x = 10;
> :type x
x : Int
> let id x = x;
> :type ident
id : ∀a. a -> a
> id x
10 : Int
> id True
True
> let inc = fn x -> x + 1
> inc 1
2 : Int
> letrec fac n = if n == 0 then 1 else n * (fac (n - 1));
> fac 7
5040
```

## TODO

* Load source from file
* Make an executable to interpret/typecheck/repl
* Basic IO
* Better error messages
* More built in data types (List, Char, String, Float, Tuple)
* Custom data types and type synonyms (records?)
* Polymorphic operators (+, -, ==...)
* Provide type signatures

Maybe someday:

* Compiler
* Pattern matching
* Module system
