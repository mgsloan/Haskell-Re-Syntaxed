Edward Kmett's reflection package allows you to turn values into types.  This
lets you add in type-based configuration of instances, letting you do things
like passing around explicit dictionaries.

See, for example,
https://github.com/ekmett/reflection/blob/master/examples/Monoid.hs

Ekmett has worked hard to make the performance overhead of this minimal,
however, using this library is a little bit daunting from a user perspective,
and having any performance overhead at all is distasteful.

Since it is now morally possible to do this, I think that it should be built
into the language.

Here's a potential syntax which re-uses implicit parameters for this purpose
(the example is based on the link above):

```haskell

-- | A dictionary describing a 'Monoid'
data Monoid_ a = Monoid_ { mappend_ :: a -> a -> a, mempty_ :: a }

-- | Values in our dynamically-constructed 'Monoid' over 'a'
data (?monoid :: Monoid_) => M a = M { runM :: a } deriving (Eq,Ord)

instance Monoid (M a) where
  mappend a b      = M $ mappend_ (monoid a) (runM a) (runM b)
  mempty a where a = M $ mempty_  (monoid a)

withMonoid :: (a -> a -> a) -> a -> M a -> a
withMonoid f z v = let ?monoid = Monoid_ f z in (runM . asProxyOf v)
```

I have not fully thought through how expressing which monoid comes from where
will work out, etc..  This is mereley a sketch.