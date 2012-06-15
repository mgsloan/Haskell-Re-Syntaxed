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
monadApplicative
  = instance Applicative m where
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
instantiate (monadReturnBind (:[]) (flip concatMap) :: Instance (Monad []))

-- Decorated TH
$(instantiate (monadReturnBind (:[]) (flip concatMap) :: Instance (Monad [])))

-- Special syntax
derive Monad [] using
  monadReturnBind (:[]) (flip concatMap)
```

Note that every definition provides the type of instance expected.  This makes
it clear what this means for our (implicit) exports, and allows for string
searches to figure out where an instance is defined.  If this was implemented
using TH, `instantiate` would have the type `Instance a -> [Dec]`.

We still haven't discussed what `Instance` actually /is/.  Being able to use
classes as parameters is conveniently provided by constraint kinds.  I'm not
sure what the value of `Instance a` should be.  Probably cleanest would be to
have it quite literally be the dictionary that's passed around for the class at
runtime. However, for the TH implementation of this idea it will almost
certainly store a list of the generated declarations.

Record (eeew) syntax could make this stuff look a tiny bit closer to building
instances based on some subset of the methods:

```haskell
instantiate (implement $ enumDict {
    toEnum   = Just fromIntegral
    fromEnum = Just (fromInteger . truncate)
  } :: Instance (Enum Float))
```

This example is from the Prelude definition for `Float`.  Interestingly,
`Double` has an identical set of method definitions!  A perfect application of
instance templates - allowing convenient implementation of `Enum` for any RealFrac
implementor.

`implement :: Implement a ctxt => a -> Instance ctxt` converts some data-
representation of an instance into the desired `Instance`.  In this case, we
update a record that's initially populated with `Nothing`s to express partial
implementations of classes.


Why?
----

* More powerful instance derivation allows us to mitigate the impact of
  historical decisions.

  - Being able to rework, say, the Numeric class hierarchy, is the main goal
    of this proposal:
    http://hackage.haskell.org/trac/ghc/wiki/DefaultSuperclassInstances
    
    As mentioned in that page, default superclass instances have been a "matter
    of consternation" for some time, as no approach to the problem has been
    satisfying enough to be implemented.  By forcing the decision of how to
    implement a class to be per-datatype, we avoid attempting to define
    typeclasses which can be implemented universally in terms of some other.
    The possibility is briefly mention, and a link to the relevant proposal
    is given:
    http://www.haskell.org/haskellwiki/Superclass_defaults
    
    This proposal, and mine, play quite nicely with constraint synonyms - instance
    templates can have a compound class constraint in the type argument.
    
    Where this proposal falls flat is that it still relies on the defaulting
    system as its mechanism, leading to strange things:
    
    > If both Class1 and Class2 have a default implementation, and Class1 is a
    > (indirect) superclass of Class2, then the default from Class1 is ignored.
    
    Also, by trying to wedge superclass defaults into the existing syntax, we
    end up with a ton of funky restrictions:
    
    > Subject to the constraint that:
    > * No class appears more than once in the list.
    > * The arguments to each class are the same.
    > * ... the superclass relation gives a connected acyclic graph with a
        single source, the most specific class in the hierarchy.
    
    This is also a weakness in the Strathyclyde Haskell Enhancement's
    implementation of default superclass instances.

  - Here's a design goal from the superclass instances write-up. It's given as
    the reason that an "Opt-In" scheme such as this is undesirable.
    
    > Design goal 1: a class C can be re-factored into a class C with a
    > superclass, without disturbing any clients.
    
    I think that this is still quite possible with the Opt-In scheme, we just
    need to make instance declarations potentially mean something quite
    different than before.  The earlier, record-based example would be
    the result of de-sugaring:

    ```haskell
    instance Enum Float where
      toEnum   = fromIntegral
      fromEnum = fromInteger . truncate
    ```

    If this sugar were implemented, then /all/ instances of Enum would pass
    through this. This can allow us to split up classes without breaking code
    (preventing the pain of things like the Eq / Show / Num split-up).


* Class defaults are broken.

  - While minimum definition requirements are often documented, they aren't
    enforced.  Using the "record" style mentioned above with instance
    templates, this can be checked.

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

  - Compared to the power and complexity of TH, this feature is very simple.

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
of the methods in the class.  Parameters that are `a` should have `f` applied
to them, and results of type `a` should have `g` applied to them.

By extending this rewriting to more cases, we can get more sophisticated
derivers.  For example, it could be specified that a result of type `f a`
should have `fmap g` applied to it.


Runtime Instances
-----------------

Treating class instance definitions as first-class values, to be returned from 
and provided to functions, might encourage investigation into being able to
provide class dictionaries at runtime.

As mentioned earlier, I'm not sure if `Instance (Num a)` should have the same
data representation that's implicitly created for `Num a =>`. For this
"run-time instances" idea, it would certainly be elegant if it did.