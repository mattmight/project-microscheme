Author: Matt Might
Site:   http://matt.might.net/

Implementing MicroScheme
========================


Your task is to implement an interpreter for MicroScheme.

MicroScheme has the following grammar:


    <prog> ::= <defs> <exp>
    
    <defs> ::= <def> <defs>
            |  
    
    <def> ::= (define (<var> <formals>) <exp>)
    
    <formals> ::= <var> <formals>
               |  
    
    <exp> ::= <integer>
           |  <var>
           |  #t | #f
           |  (lambda (<formals>) <exp>)
           |  (let ([<var> <exp>] ...) <exp>)
           |  (if <exp> <exp> <exp>)
           |  (set! <var> <exp>)
           |  (begin <exp> ...)
           |  (<prim> <exp> ...)
           |  (<exp> <exp> ...)
           |  (call/cc <exp>)
    
    <prim> ::= +  |  -  |  *  |  = 
            |  void  


Your interpreter should execute the script on stdin and print the final result. 


Tips
----

There are a variety of viable implementation strategies:

 * You could construct a [CEK or CESK machine][CEK] directly for the language 
   itself.

 * You could also desugar and/or [a-normalize][ANF] the language, which will
   allow you to construct a much [simpler CESK machine][CESK].

 * You could also [CPS transform][CPS] the language, which allows you to build a
   substantially simpler CES machine.  

I strongly recommend that you use Racket (or some dialect of Scheme) for this
project, since the `(read)` command handles lexing and parsing for you.

You may find the stub code provided in [microscheme.rkt][stub] useful, but you
are not required to use it.


Testing
-------

With Racket installed, you can build the expected results for the test cases.

With [s-diff] installed, you can compare your interpreter to the expected
results with:

    make test


Restrictions
------------

You may not use the `eval` command in any Scheme distribution,
including Racket.


[ANF]:    http://matt.might.net/articles/a-normalization/ 

[CPS]:    http://matt.might.net/articles/cps-conversion/

[CEK]:    http://matt.might.net/articles/cek-machines/

[CESK]:   http://matt.might.net/articles/cesk-machines/

[stub]:   src/microscheme.rkt

[s-diff]: https://github.com/mattmight/s-exp-diff
