Instance Templates
==================

This goes a little bit beyond "re-skinning" Haskell, however is fairly
equivalent to introducing TH functions of the variety that are already pretty
popular: deriving class instances.

The purpose of these declarations is to provide a mechanism for expressing how
a set of interfaces can be expressed in terms of some other interface.  This
allows us to restructure class hierarchies without breaking client code, and
reduce code duplication found in boilerplate instances.

```haskell
deriving class PreOrder a where
  (<=) :: a -> a -> Bool

  instance Eq a where
    x == y = (x <= y) && (y <= x)

  instance Ord a where
    (<=) = (<=)
```

The immediately nested `(<=)` signature declares a "parameter" of the "class
deriver".  These are considered parameters, because they are only used to
specialize the generated instances, and don't get exported.

The `(<=)` nested inside `instance Ord` is not ambiguous with the outer one,
because the deriver parameters shadow the methods defined within (method
definitions can reference each other, though).

When utilized to generate instances, "PreOrder" is referencing the instance
template.  What should it mean elsewhere?  Well, it'd be nice to still have
the property that instance heads can be used as constraints in polymorphic
types.  As such, I think it's reasonable for the above definition to
implicitly create the following constraint-kind synonym:

```haskell
type PreOrder a = (Eq a, Ord a)
```

In order to use the above declaration, we write code that looks like this:

```haskell
instance PreOrder Bool where
  False <= _ = True
  True  <= x = x
```

This instance would then, at compile time, be expanded into:

```haskell
instance Eq a where
  x == y = (x <= y) && (y <= x)
   where
    False <= _ = True
    True  <= x = x

instance Ord a where
  (<=) = (<=)
   where
    False <= _ = True
    True  <= x = x
```

It's somewhat ugly that the provided parameters would be duplicated among
every single method of the generated instances, but this is the trivial,
definitional desugaring.  The compiler could certainly do something more
clever.  This desugaring justifies the scoping rules mentioned earlier. I'm
not even sure if "templates" should be instantiated in the way that their name
suggests.  Since there is certainly finite usage of them in a given program,
it's reasonable to literally instantiate them.  At once, it might be more
efficient to only compile the template, and provide the parameters at runtime.

Why?
====

* It's simple.  We're just supplying values to a generic instance, to create a
  specific one, and these parameters are referentially transparent.

* More powerful instance derivation allows us to mitigate the impact of
  historical decisions.

  Being able to rework, say, the Numeric class hierarchy, is the main goal of
  this proposal (and those that came before):
  http://hackage.haskell.org/trac/ghc/wiki/DefaultSuperclassInstances
    
  As mentioned in that page, default superclass instances have been a "matter
  of consternation" for some time, as no approach to the problem has been
  satisfying enough to be implemented.  By forcing the decision of how to
  implement a class to be per-datatype, we avoid attempting to define
  typeclasses which can be implemented universally in terms of some other.

* Avoidance of TH.

  - People have observed many things that are wrong with TH / mis-aligned with
    Haskell philosophy.
    http://stackoverflow.com/questions/10857030/whats-so-bad-about-template-haskell/

  - This feature can be implemented as a TH library, though requiring special
    delineation around usages.  However, the error messages and potential for
    analysis by tools would be impaired.  By making it a language feature, we
    can conquer even more of the useful regions of macro-expansion design space
    and reduce the necessity of TH.

  - Compared to the power and complexity of TH, this feature is very simple,
    and we can rely on the reasoning power of referential transparency.


Examples
========

Splitting out Eq / Show
-----------------------

With GHC 7.4, one of the first breaking changes (in a while, anyway) was made
to the Prelude - removing the Eq and Show superclass constraints.  Here's how
this feature would have made the change non-breaking:

```haskell
-- In Prelude

import qualified NewPrelude as N

deriving class (Show a, Eq a) => Num a where
  instance N.Num a

deriving class (Num a, Ord a) => Real a where
  instance N.Real a

deriving class (Real a, Enum a) => Integral a where
  instance N.Integral a

deriving class (Num a) => Fractional a where
  instance N.Fractional a

deriving class (Real a, Fractional a) => RealFrac a where
  instance N.RealFrac a
```

Here, the instances which contain no "where" clause indicate that the methods
of the instance are implicitly made into parameters of the deriver.  I'm not
sure if this is a good syntactic choice, but a mechanism for doing this is
_very_ handy for the situations in which you wish to modify a class hierarchy
without breaking clients.

Eq / Show is a fairly minimal change to the numeric hierarchy, but it required
a lot of declarations because it happened at the root of the hierarchy.

Redundant Enum instances for RealFloat
--------------------------------------

```haskell
instance  Enum Float  where
    succ x           =  x+1
    pred x           =  x-1
    toEnum           =  fromIntegral
    fromEnum         =  fromInteger . truncate   -- may overflow
    enumFrom         =  numericEnumFrom
    enumFromThen     =  numericEnumFromThen
    enumFromTo       =  numericEnumFromTo
    enumFromThenTo   =  numericEnumFromThenTos

instance  Enum Double  where
    succ x           =  x+1
    pred x           =  x-1
    toEnum           =  fromIntegral
    fromEnum         =  fromInteger . truncate   -- may overflow
    enumFrom         =  numericEnumFrom
    enumFromThen     =  numericEnumFromThen
    enumFromTo       =  numericEnumFromTo
    enumFromThenTo   =  numericEnumFromThenTo
```

A perfect application of instance templates!  These two instance declarations
are identical. In general, we can use this as a template for a potential
way of getting an implementation of `Enum` for any `RealFrac` implementor.
Here's the implementation using templates:

```haskell
deriving class RealFrac a => RealFracEnum a where
  instance Enum a where
    succ x           =  x+1
    pred x           =  x-1
    toEnum           =  fromIntegral
    fromEnum         =  fromInteger . truncate   -- may overflow
    enumFrom         =  numericEnumFrom
    enumFromThen     =  numericEnumFromThen
    enumFromTo       =  numericEnumFromTo
    enumFromThenTo   =  numericEnumFromThenTo

instance RealFracEnum Float

instance RealFracEnum Double
```

Fine Grained Numerics
---------------------

A more fine-grained splitting of the numeric hierarchy might look something
like this:

```haskell
class Addable a where
  (+) :: a -> a -> a

class Multiplicable a where
  (*) :: a -> a -> a

class Subtractable a where
  (-) :: a -> a -> a

class Negateable a where
  negate :: a -> a

class Absable a where
  abs :: a -> a

class Signumable a where
  signum :: a -> a

class FromIntegerable a where
  fromInteger :: a -> a

deriving class Num a where
  instance Addable         a
  instance Multiplicable   a
  instance Subtractable    a
  instance Negateable      a
  instance Absable         a
  instance Signumable      a
  instance FromIntegerable a
```

This can be made even more general (however possibly breaking code):

```haskell
-- ...

class Subtract a b where
  type SubtractResult a b :: *
  (-) :: a -> b -> SubtractResult a b

class Negate a where
  type NegateResult a :: *
  negate :: a -> NegateResult a

-- ...

deriving class Subtractable a where
  (-) :: a -> a -> a
  instance Subtract a a where
    type SubtractResult a b = a
    (-) = (-)

deriving class Negateable a where
  (-) :: a -> a -> a
  instance Subtract a a where
    type SubtractResult a b = a
    (-) = (-)

-- ...

deriving class Num a where
  instance Addable         a
  instance Multiplicable   a
  instance Subtractable    a
  instance Negateable      a
  instance Absable         a
  instance Signumable      a
  instance FromIntegerable a
```

Note that the Num deriver remained unchanged, despite all of the methods now
having the most general type that's still (somewhat) useful!  Unfortunately,
this particular example might break some polymorphic code.  However, the effect
shouldn't be that extensive.

Functor - Applicative - Monadic
-------------------------------
```haskell
module NewPrelude where

-- Similar to http://www.haskell.org/haskellwiki/Functor-Applicative-Monad_Proposal

class Functor f where
  map :: (a -> b) -> f a -> f b
 
class Functor f => Applicative f where
  return :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
  (*>) :: f a -> f b -> f b
  (<*) :: f a -> f b -> f a

  a *> b = map (const id) a <*> b
  a <* b = map const a <*> b
 
class Applicative m => Monadic m where
  (>>=) :: m a -> (a -> m b) -> m b
  x >>= f = join $ map f x

  join :: m (m a) -> m a
  join x = x >>= id
 
class Monadic m => MonadFail m where
  fail :: String -> m a


-- We would then have the original Prelude export the following:

type Monad m = (Functor m, Applicative m, Monadic m, MonadFail m)

deriver Monad m where
  (>>=)  :: m a -> (a -> m b) -> m b
  (>>)   :: m a ->       m b  -> m b
  return :: a      -> m a
  fail   :: String -> m a

  m >> k = m >>= \_ -> k
  fail = error

  instance Functor m where
    map f = (>>= return . f)

  instance Applicative m where
    return = return
    l <*> r = l >>= \f ->
              r >>= \x -> return (f x)

  instance Monadic m where
    (>>=) = (>>=)

  instance MonadFail m where
    fail = fail
```


API Deltas
==========
http://www.mgsloan.com/wordpress/?p=219

In this somewhat-rough, yet extensive blog post, I describe a solution to
dependency hell.  It's not a new one - the idea is to export compatibility
modules.  This frees the library developer to develop the API without worrying
as much about client code.

This offers a different perspective of what it can mean to write Haskell
modules of a particular form - they can express "API Deltas".  In other words,
they can introduce a set of definitions, in terms of some other API, that
encode the rewriting necessary to target that other API.

TODO: enumerate all of the possible forms of API differences, and show which
ones this feature gains us.


"Problems"
==========

No potential solution to the "default superclass instances" is without its
trade-offs.  I think that the described solution is a straightforward,
understandable solution to the problem, that buys a lot of power, with
comparatively minimal issues:

* We can now make instances mean something different, and generate more
  class instances than before.  This has a few implications:

    - The generated instances might conflict with those already defined in the
      module.  In this case, I think that this is a very reasonable mechanism
      for "opt-ing out" of a particular generated instance.  However, it could
      be unexpected, so a pragma-suppressible warning should be generated.

    - The instances a particular module exports is now dependent on the way
      the classes are defined in its imports. This is not as much of an issue
      as it sounds - an instance of "Monad" will still mean we have an
      instance of "Monad" - be it a normal or compound class constraint.

      However, this is an issue when it comes to orphan instances, as identical
      library / client code could have an instance clash, given a different
      set of definitions for the library's dependencies.  I think that this is
      acceptable, as orphan instances are known to be dangerous, and many
      instance templates would not have this sort of behavior.

* Cannot "combine" multiple instances, using their methods as parameters to
  the instance template.  (See "API Deltas" section above)

* Ideally we'd be able to seamlessly use old code with our new typeclasses, in
  concordance with "Design goal 1" mentioned in the "Other Proposals" section.
  However, this means that this feature would need to only have a LANGUAGE
  extension option (e.g. -XInstanceTemplates) for the modules defining
  instance templates.

  Are we comfortable changing the meaning of code without additional pragma,
  in the event that the dependencies specify this pragma?  Is there any
  precedent for language extensions doing this?


Bonus Feature - Scope-Restricted Weak Typing
============================================

This is not at all a crucial point of my proposal, however, it is the kind of
thinking about instances that falls out of having a capability like this.

It's very tempting to give your language the 'intelligence' to implicitly apply
a function in order to resolve what would otherwise be a type error.  Common
examples of weak typing found in other languages are `Int -> Float`,
`Bool -> Int`, `Int -> String`, `String -> Int` and even `Float -> Int`.

The main example of this is anything that's newtype-ish - where we have
(a -> b) and (b -> a).  An instance template could take these two functions and
yield the instance that would have been generated if you could inject a custom
constructor / destructor into GeneralizedNewtypeDeriving.  For example, Num:

```haskell
deriving Num b => BijNum a b where
  f :: a -> b
  g :: b -> a

  instance Num a where
    x + y         = g (f x + f y)
    x * y         = g (f x * f y)
    x - y         = g (f x - f y)
    negate      x = g (negate      (f x))
    abs         x = g (abs         (f x))
    signum      x = g (signum      (f x))
    fromInteger x = g (fromInteger (f x))
```

We can automatically generate this definition by processing the type signatures
of the methods in the class.  Parameters that are `a` should have `f` applied
to them, and results of type `a` should have `g` applied to them.

By extending this rewriting to more cases, we can get more sophisticated
templates.  For example, it could be specified that a result of type `f a`
should have `fmap g` applied to it.  By giving a notation for specifying this
rewriting, we are conceptually introducing the convenience of weak-typing, but
properly scoped to a known set of definitions / value junctures.


Relationship to Other Proposals
===============================

(This section is incomplete)

The question might be asked "Why wasn't this thought of before?".  The answer
is "I'm not sure", but I think that it likely has been thought about before,
just disregarded due to some preconceived notions of a desirable mechanism.
By focusing on extending the typeclass system directly, we end up with a very
complicated set of trade-offs, that are tough to navigate.


Here's a design goal from the superclass instances write-up. It's given
as the reason that an "Opt-In" scheme such as this is undesirable,
without much further explanation.
  
> Design goal 1: a class C can be re-factored into a class C with a
> superclass, without disturbing any clients.

I think that this is still quite possible with the Opt-In scheme, we
just need to make instance declarations potentially mean something
quite different than before (when applied to constraint synonyms).

This can allow us to split up classes without breaking code
(preventing the pain of things like the Eq / Show / Num split-up).


http://www.haskell.org/haskellwiki/Superclass_defaults

This proposal, and mine, play quite nicely with constraint synonyms -
instance templates can have a compound class constraint in the type
argument.

Where this proposal falls flat is that it still relies on the
defaulting system as its mechanism, leading to strange things:

> If both Class1 and Class2 have a default implementation, and Class1
> is a (indirect) superclass of Class2, then the default from Class1
> is ignored.

Also, by trying to wedge superclass defaults into the existing syntax,
we end up with a ton of funky restrictions:

> Subject to the constraint that:
> * No class appears more than once in the list.
> * The arguments to each class are the same.
> * ... the superclass relation gives a connected acyclic graph with a
    single source, the most specific class in the hierarchy.

This is also a weakness in the Strathyclyde Haskell Enhancement's
implementation of default superclass instances.