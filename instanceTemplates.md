Instance Templates
==================

This goes a little bit beyond "reskinning" Haskell, however is fairly
equivalent to introducing TH functions of the variety that are already pretty
popular: deriving class instances.

```haskell
monadReturnBind :: (a -> m a)
                -> (m a -> (a -> m b) -> m b)
                -> Instance (Monad m)
monadReturnBind b r 
  = instance Monad m where
      m >>= k = b
      return  = r
      m >> k  = m >>= \_ -> k
      fail s  = error s


monadApplicative :: Monad m
                 => Instance (Applicative m)
monadApplicative = instance Applicative m where
  pure = return
  (<*>) = ap
```

I'm not sure what to call these, so for now will use "instance templates".
I also like the term "instantiators", as this does not have a distinct meaning
within Haskell's language context.  Overloading a term that's useful to
describe libraries might be confusing, though.  It might make sense to use
the term "deriver" if these become the dominant method of instance derivation.

One question is the syntax used to invoke instance templates.  Here are a few
possibilities:

```haskell
-- TH style (undecorated, top-level invocation)
monadReturnBind (:[]) (flip concatMap) :: Instance (Monad [])

-- Decorated TH
$(monadReturnBind (:[]) (flip concatMap) :: Instance (Monad []))

-- Using type declarations to get the instance we want
monadApplicative :: Instance (Applicative [])

-- Special syntax (probably a bad example)
derive Monad [] using
  monadReturnBind (:[]) (flip concatMap)
```

Note that every definition provides the type of instance expected.  This makes
it clear what this means for our (implicit) exports, and allows for string
searches to figure out where an instance is defined.

We still haven't discussed what "Instance" actually /is/.  At first, I
thought it would simply have kind "Constraint -> *".  I'm not sure what the
value of "Instance a" should be.  Probably cleanest would be to have it quite
literally be the dictionary that's passed around for the class at runtime, in
order to satisfy "Num a =>" constraints. However, for the TH implementation of
this idea it will almost certainly store a list of the generated declarations.



Here's why this idea is interesting:

* More powerful instance derivation allows us to mitigate historical decisions.

  - Being able to rework, say, the Numeric class hierarchy, is the main goal
    of this proposal:
    http://hackage.haskell.org/trac/ghc/wiki/DefaultSuperclassInstances

  - In this post, Luke Palmer points out that without orphan instances, we can
    do default-superclassing.

    http://lukepalmer.wordpress.com/2009/01/25/a-world-without-orphans/

    The problem with this is that orphan instances can be nice – in the
    comments of the above post, augustss points out that newtype wrappers are
    clunky. Orphan instances lead to the same benefits and problems as multiple
    inheritance: the potential for combination of modular code, at the cost of
    the potential for conflict.

  - By forcing the decision per-datatype, we end up with the same situation
    regarding orphans - calling these instance-generating functions 

* Class defaults are broken.

  - While minimum definition requirements are often documented, they aren't
    enforced.

  - They create somewhat OOP-ish inheritance of implementation. While handy,
    this can encourage some very undesirable class hierarchy designs.

  - They encourage sticking methods into the class that wouldn't be there
    other than to provide optimizations for specific instance cases.

* Avoidance of TH.

  - People have observed many things that are wrong with TH / mis-aligned with
    Haskell philosophy.
    http://stackoverflow.com/questions/10857030/whats-so-bad-about-template-haskell/

  - While this feature could be implemented as a TH library (which I hope to
    write), by making it a language feature, we can conquer the majority of the
    useful / reasonable TH design space.


Not avoiding TH
---------------

I probably shouldn't mention this idea, due to the apparently heretical nature
of TH and its macro-hood.  However, it's occurred to me that many varieties of
these instance templates have regular structure.

The main example of this is anything that's newtype-ish - where we have
(a -> b) and (b -> a).  An instance template could take these two functions and
yield the instance that would have been generated if you could inject a custom
constructor / destructor into GeneralizedNewtypeDeriving.  For example, Num:

```haskell
wrapNum :: Num b
        => (a -> b)
        -> (b -> a)
        -> Instance (Num a)
wrapNum f g = instance Num a where
  x + y         = g (f x + f y)
  x * y         = g (f x * f y)
  x - y         = g (f x - f y)
  negate      x = g (negate      (f x))
  abs         x = g (abs         (f x))
  signum      x = g (signum      (f x))
  fromInteger x = g (fromInteger (f x))
```

We can automatically generate this definition by processing the type signatures
of the methods in the class.  Parameters that are "a" should have "f" applied
to them, and results of type "a" should have "g" applied to them.

By extending this rewriting to more cases, we can get more sophisticated
derivers.  For example, it could be specified that a result of type "f a"
should have "fmap g" applied to it.


Runtime Instances
-----------------

Treating class instance definitions as first-class values, to be returned from 
and provided to functions, might encourage investigation into being able to
provide class dictionaries at runtime.

As mentioned earlier, I'm not sure if "Instance (Num a)" should have the same data representation 
that's implicitly created for "Num a =>". For this "run-time instances" idea,
it would certainly be elegant if it did.  However, for the TH implementation of
this idea it will almost certainly a list of the generated declarations.