# Lum

Inspired by [lamb](https://github.com/tsoding/lamb) from [TSoding](https://github.com/tsoding)

## Examples

```
λ> :debug \f.(\x.f (x x)) (\x.f (x x))
!> Debugging: λf.(λx.(f (x x)) λx.(f (x x)))
 > Press Enter to continue evaluation or type 'quit' to exit debugging
#> λf.(λx.(f (x x)) λx.(f (x x)))
->
#> λf.(f (λx.(f (x x)) λx.(f (x x))))
->
#> λf.(f (f (λx.(f (x x)) λx.(f (x x)))))
->
#> λf.(f (f (f (λx.(f (x x)) λx.(f (x x))))))
->
#> λf.(f (f (f (f (λx.(f (x x)) λx.(f (x x)))))))
->
#> λf.(f (f (f (f (f (λx.(f (x x)) λx.(f (x x))))))))
->
#> λf.(f (f (f (f (f (f (λx.(f (x x)) λx.(f (x x)))))))))
->
#> λf.(f (f (f (f (f (f (f (λx.(f (x x)) λx.(f (x x))))))))))
->
#> λf.(f (f (f (f (f (f (f (f (λx.(f (x x)) λx.(f (x x)))))))))))
->
#> λf.(f (f (f (f (f (f (f (f (f (λx.(f (x x)) λx.(f (x x))))))))))))
```

## Quick Start

To run use `$ cargo run` in console.

## Syntax

- Syntax is based on untyped Lambda calculus

## Binding

Use `:let` command to create binding:

```

λ> :let id = \x.x
!> Created binding 'id'
λ> id 420
λ> 420
λ> :let Y = \f.(\x.f (x x)) (\x.f (x x))
λ> :list
 > id = λx.x
 > Y = λf.(λx.(f (x x)) λx.(f (x x)))
```

Bindings are lazy! Means they won't evaluate until they used in expression

## Commands

- `:quit` exit from REPL
- `:ast <expr>` render AST of the expression
- `:debug <expr>` step-by-step evaluation of the expression
- `:let <name> = <expr>` save expression in a associative name (binding)
- `:list` list all existing bindings
- `:delete <name>` delete binding by it's name
