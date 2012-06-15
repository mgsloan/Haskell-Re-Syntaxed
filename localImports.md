Where declarations already allow you to pretend like you are introducing a
new local namespace / module - why not allow additional varieties of
top-level declarations?

Local imports
=============

This is the #1 thing that I want for local declarations, as this would allow
for the usage of eDSLs without having the names used be unqualified
throughout the module.

```haskell
fooLens = foo . bar
 where
  import Control.Category
```

This leads to the problem that now we can't look at the beginning of a module
to know what its import dependencies are.  As such, my proposal would be to
have the local import reference a qualified import.  E.g:

```haskell
import qualified Control.Category as Cat

fooLens = foo . bar
 where
  import Cat

-- (Which de-sugars to)
fooLens = foo `Cat.(.)` bar
```

This has the added benefit that you can bind multiple imports to a qualified
name.  Then, a single local important can load up a set of names that
constitute one logical domain of things you're working with.

In order to fix up some parsing ambiguities, GHC now supports a new syntax for
qualified operators (used above):
http://www.haskell.org/ghc/docs/6.12.3/html/users_guide/syntax-extns.html#new-qualified-operators

This is definitely a good change, however it incurs an additional overhead of 4
characters `\`(A.(+)\`` vs `A.+` for infix qualified operators.  This is too much for
things that were meant to be concise, combining symbols, which is why you don't
see very many qualified operators.

By having local imports, we can encourage combinator-rich DSLs, while not
polluting our namespace with them.