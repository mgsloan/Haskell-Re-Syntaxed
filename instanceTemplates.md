Instance Templates
==================

This goes a little bit beyond "reskinning" Haskell, however is fairly
equivalent to introducing TH functions of the variety that are already pretty
popular: deriving class instances.

```haskell
deriving class PreOrder a where
  (<=) :: a -> a -> Bool

  instance Eq a where
    x == y = (x <= y) && (y <= x)

  instance Ord a where
    (<=) = (<=)
```

The immediately nested `(<=)` signature specifies the "parameters" of the
"class deriver".  These are considered parameters, because they are only used
to specialize the generated instance, and aren't exported.

The `(<=)` nested inside `instance Ord` is not ambiguous with the outer one,
because the deriver parameters shadow the methods defined within (method
definitions can reference eachother, though).

It's also useful to be able to refer to the deriver by the same name with
which it is invoked.  Therefore, the above definition would implicitly create
the following constraint-kind synonym:

```haskell
type PreOrder a = (Eq a, Ord a)

-- Or, equivalently, due to superclasses:
type PreOrder a = Ord a
```

In order to use the above declaration, we write code that looks like this:

```haskell
instance PreOrder Bool where
  False <= _ = True
  True  <= x = x
```

Something to note is that this instance could not have been written before,
because `PreOrder` is a constraint synonym.  This means that this feature does
not cause any ambiguity.  If you know which names are constraint synonyms, and
which are classes, then it is clear which instances invoke a derivation.

The above instance would be expanded into:

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
definitional de-sugaring.  The compiler could certainly do something more
clever.  This de-sugaring justifies the scoping rules mentioned earlier.

Another issue is that if we now generate more instances than before, then
there will be an ambiguity with any old declarations, particularly orphans.  I
think that it's reasonable to assume that if an instance is given in the same
module, then that instance should be given priority (perhaps generating a
WARNING that can be supressed with a pragma). As far as orphans go, they are
known to be dangerous, so it is fine to break orphan assumptions.

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

  - Compared to the power and complexity of TH, this feature is very simple.
    It's referentially transparent, hygenic expansion, as opposed to 


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

Here, the empty instances are indicating that the methods of the instance are
implicitly made into parameters of the deriver.

Eq / Show is a fairly minimal change to the numeric hierarchy, but it
required a lot of declarations because it happened at the root of the
hierarchy.

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
Here's the implemention using derivers:

```haskell
deriving class RealFrac a => FloatEnum a where
  instance Enum a where
    succ x           =  x+1
    pred x           =  x-1
    toEnum           =  fromIntegral
    fromEnum         =  fromInteger . truncate   -- may overflow
    enumFrom         =  numericEnumFrom
    enumFromThen     =  numericEnumFromThen
    enumFromTo       =  numericEnumFromTo
    enumFromThenTo   =  numericEnumFromThenTo

instance FloatEnum Float

instance FloatEnum Double
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
class Add a b where
  type AddResult a b :: *
  (+) :: a -> b -> AddResult a b

class Subtract a b where
  type SubtractResult a b :: *
  (-) :: a -> b -> SubtractResult a b

class Negate a where
  type NegateResult a :: *
  negate :: a -> NegateResult a

class Abs a where
  type AbsResult a :: *
  abs :: a -> Abs a

class Signum a where
  type SignumResult a :: *
  signum :: a -> SignumResult a

class FromInteger a where
  type FromIntegerResult :: *
  fromInteger :: a -> FromIntegerResult a

deriving class Addable a where
  (+) :: a -> a -> a
  instance Add a a where
    type AddResult a b = a
    (+) = (+)

deriving class Subtractable a where
  (-) :: a -> a -> a
  instance Subtract a a where
    type SubtractResult a b = a
    (-) = (-)

deriving class Negateable a where
  negate :: a -> a
  instance Negate a where
    type NegateResult a b = a
    negate = negate

deriving class Absable a where
  abs :: a -> a
  instance Abs a a where
    type AbsResult a = a
    abs = abs

deriving class Signumable a where
  signum :: a -> a
  instance Signum a where
    type SignumResult a = a
    signum = signum

deriving class FromIntegrable a where
  fromIntegral :: a -> a
  instance FromIntegral a where
    type FromIntegralResult a = a
    fromIntegral = fromIntegral

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
having the most general type that's still (somewhat) useful!  Unfortuantely,
this might break some polymorphic code that has explicit signatures.  Also,
polymorphic signatures might get much more complicated.

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


Bonus Feature - Scope-Restricted Weak Typing
--------------------------------------------

This is not at all a crucial point of my proposal, however, it is the kind of
thinking about instances that falls out of having a capability like this.

It's very tempting to give your language the 'intelligence' to implicitly apply
a function in order to resolve what would otherwise be a type error.  Common
examples of weak typing found in other languages are `Int -> Float`,
`Bool -> Int`, `Int -> String`, `String -> Int` and even `Float -> Int`.

Conveniences like this are 

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
derivers.  For example, it could be specified that a result of type `f a`
should have `fmap g` applied to it.  By giving a notation for specifying this
rewriting, we are conceptually introducing the convenience of weak-typing, but
properly scoped to a known set of defintions / value junctures.


Relationship to Other Proposals
-------------------------------

(This section is incomplete)

The question might be asked "Why wasn't this thought of before?".  The answer
is "I'm not sure", but I think that it likely has been thought about before,
just disregarded due to some preconceived notions of a desirable mechanism.
By focussing on extending the typeclass system directly, we end up with a very
complicated set of tradeoffs, that are tough to navigate.


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

