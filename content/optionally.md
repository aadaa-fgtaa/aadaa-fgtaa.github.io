+++
title="Checking for uncheckable: optional constraints"
date=2021-04-17

[taxonomies]
tags = ["haskell"]
+++

# The challenge

Can we check if instance exists and acquire its dictionary without defining boilerplate instances by
hand, like [constraint-unions](https://github.com/rampion/constraint-unions) suggests, or using
template haskell to generate them, like [ifctx](https://github.com/mikeizbicki/ifcxt) does?

In other words, can we define

```haskell
class Optionally c where
  optionalDict :: Maybe (Dict c)
```

that is always satisfied for concrete `c`, like e.g. `Typeable` does, such that `optionalDict` would
return `Just Dict` if constraint `c` is satisfied at call side?

I found a solution that I think is interesting and in some sense beautiful (and at the same time an
ugly hack), so I decided to write about it.

> __DISCLAIMER__ This trick definitely shouldn't be used for anything serious, it's just an abuse of
> ghc unspecified behaviour. Don't repeat this at home!

> __NOTE__ I assume that you are familiar with ghc core, especially with the representation of type
> classes and instances. If you are not, [this talk](https://www.youtube.com/watch?v=fty9QL4aSRc) by
> Vladislav Zavialov may be a good introduction.

# Constraint solver and known dictionaries

As you probably know, ghc is usually very stubborn about constraints: once constraint is introduced
as wanted, ghc would solve it or throw compilation error if failed. This makes checking for
instance's existence difficult.

Still there are some cases when ghc behaves differently depending on instance being defined, so
these both cases don't cause compilation error. One of the cases is that ghc prefers to solve a
constraint with a top-level dictionary rather than local one passed to a function, as long as
optimisations are enabled.

For example, in the following code

```haskell
test :: Eq Integer => Dict (Eq Integer)
test = Dict
```

ghc with `-O` would just ignore passed constraint and use top-level `$fEqInteger` instead

```haskell
test1 :: Dict (Eq Integer)
test1 = Dict $fEqInteger

test :: Eq Integer => Dict (Eq Integer)
test = \ _ -> test1
```

Using this fact we can check if an instance exists by defining such `test` function, passing bottom
as a dictionary argument to it and checking if an exception occurrs. If it did, the constraint is
probably unsatisfied, otherwise we get a `Dict` of it.

But making `optionalDict` accept `c => Dict c` as an argument would make api awful - every caller of
function that uses `optionalDict` would need to pass `Dict` as an argument to it. Luckily we can
avoid this by using `QuantifiedConstraints` - constraint solving for them works the same way and
known dictionary would be used when possibility.

To illustrate this, in the following code

```haskell
test :: ((Ord Int => Eq Int) => r) -> r
test f = f
```

ghc would ignore `Ord Int` and pass `\_ -> $fEqInt` to `f`

```haskell
test1 :: Ord Int => Eq Int
test1 = \ _ -> $fEqInt

test :: forall r. ((Ord Int => Eq Int) => r) -> r
test = \ (@ r) (f :: (Ord Int => Eq Int) => r) -> f test1
```

# .hs-boot trick

Now we understand what the constraint of `optionalDict` should be - something of form `a => b` so
that it is always satisfied. An obvious choice would be `c => c`, but unluckily such constraint
loops the typechecker, so instead we would use `Hold c => c`, with `Hold` defined as

```haskell
-- for some reason syntax highlighting in zola breaks if I omit those `where`s
class c => Hold c where
instance c => Hold c where
```

When there is a dictionary for `c` in scope, such constraint would be solved with `\_ -> cDict`,
otherwise with `$p1Hold`, a selector that extracts first superclass from `Hold`.

But trying to add this constraint to `optionalDict` we would immediately hit a problem: ghc fairly
considers such constraint trivial. This means that if we would try to use this constraint inside of
`optionalDict`, ghc would just rignore passed constraint and use `$p1Hold` instead, exactly like it
ignored useless givens in the previous section.

For example, if we would write something like this

```haskell
useHold :: forall c . (Hold c => c) => Dict (Hold c) -> Dict c
useHold = sub @(Hold c) @c

sub :: (a => b) => Dict a -> Dict b
sub Dict = Dict
```

generated core for `useHold` would be

```haskell
useHold :: forall (c :: Constraint). (Hold c => c) => Dict (Hold c) -> Dict c
useHold = \ (@ c) _ (d :: Dict (Hold c)) -> case c of Dict i -> Dict (i `cast` <Co:2>)
```

But we don't want this to happen - we are going to distinguish defined instance from undefined by
looking at the passed `Hold c => c` constraint. Currently, we always work with `$p1Hold` which gives
us no information at all.

To prevent ghc from dropping this constraint we need to make it non-trivial, but at the same time we
need it to be trivial at the call side so it would be always solved. So we should make ghc forget that
`c` is the supreclass of `Hold c` when defining `optionalDict`, but at call side it should know that again.

We cannot control exporting of superclasses, but there is another possibility: instead of making ghc
forget about `Hold`s superclass we can make ghc not-yet-know about it by putting forward declaration
of `Hold` into `.hs-boot` file and giving it no superclass.

Module defining `optionalDict` would `{-# SOURCE #-}`-import `Hold` so it would be an abstract class,
whereas at useside `Hold` would be imported normally and `Hold c => c` would be trivial again.

So we would have something like this

```haskell
-- Data/Constraint/Optional/Hold.hs-boot

class Hold (c :: Constraint) where

-- Data/Constraint/Optional/Impl.hs

import {-# SOURCE #-} Data.Constraint.Optional.Hold

useHold :: forall c . (Hold c => c) => Dict (Hold c) -> Dict c
useHold = sub @(Hold c) @c

sub :: (a => b) => Dict a -> Dict b
sub Dict = Dict

-- Data/Constraint/Optional/Hold.hs

import Data.Constraint.Optional.Impl

class c => Hold c where
instance c => Hold c where
```

Now `useHold` is what we wanted it to be:

```haskell
useHold :: forall (c :: Constraint). (Hold c => c) => Dict (Hold c) -> Dict c
useHold = \ (@ c) (d :: Hold c => c) -> sub d
```

# Manipulating dictionaries

Now we should just check if passed constraint actually uses `Hold c` argument by passing `undefined`
dictionary to it and check if exception occures. If it doesn't, we would get `Dict c` and return it,
otherwise the constraint doesn't seem to be satisfied, so we simply return `Nothing`.

Everything we need is an `undefined` dictionary and a function to force the dictionary stored inside
of `Dict`.

Speaking core we want something like this

```haskell
errorDict :: Dict c
errorDict = Dict undefined

forceDict :: Dict c -> ()
forceDict = \ (@ c) (d :: Dict c) -> case d of Dict c -> c `seq` ()
```

Though in haskell we cannot manipulate the constraint directly, those functions can be defined
easily using some `unsafeCoerce`s

```haskell
newtype Gift c a = Gift { unGift :: c => a }

data NoInstanceError = NoInstanceError
  deriving stock Show
  deriving anyclass Exception

errorDict :: Dict c
errorDict = unsafeCoerce (Gift Dict :: Gift c (Dict c)) (throw NoInstanceError)

forceDict :: forall c . Dict c -> ()
forceDict Dict = unGift @c $ unsafeCoerce (`seq` ())
```

> __NOTE__: Some explanations of this trick can be found e.g.
> [here](https://www.schoolofhaskell.com/user/thoughtpolice/using-reflection) or
> [here](https://stackoverflow.com/questions/17793466/black-magic-in-haskell-reflection).

This gives exactly the core we wanted

```haskell
ex :: SomeException
ex = $fExceptionNoInstanceError_$ctoException NoInstanceError

errorDict2 :: Any
errorDict2 = raise# ex

errorDict1 :: forall (c :: Constraint). Dict Any
errorDict1 = \ (@ c) -> Dict errorDict2

errorDict :: forall (c :: Constraint). Dict c
errorDict = errorDict1 `cast` <Co:24>

forceDict :: forall (c :: Constraint). Dict c -> ()
forceDict
  = \ (@ c) (d :: Dict c) ->
      case d of { Dict c ->
      case c `cast` <Co:15> of { __DEFAULT -> () }
      }
```

> __NOTE__ If you would look closely at `forceDict` you would probably immediately see an issue with
> it: if `c` is represented with newtype, e.g. is a single method class, `forceDict` would force
> that method instead of the dictionary. It would be very bad if method is bottom or expensive to
> compute. I don't see any way to fix it so I would just hope that this case is corner enough to
> ignore.

# Actually implementing optionalDict

With everything defined above we can easily define our `optionalDict`

```haskell
optionalDict :: forall c . (Hold c => c) => Maybe (Dict c)
optionalDict = unsafeDupablePerformIO $ catch
  do evaluate (forceDict c) $> Just c
  do \NoInstanceError -> pure Nothing
  where
    c :: Dict c
    c = sub @(Hold c) @c errorDict
-- I'm actually not sure if NOINLINE is really needed, but there is 'errorDict' inside
-- so I want to be sure that ghc wouldn't pass that dictionary somewhere else.
{-# NOINLINE optionalDict #-}
```

And some helpers for it

```haskell
isSatisfied :: forall c . (Hold c => c) => Bool
isSatisfied = isJust $ optionalDict @c

maybeC :: forall c r . (Hold c => c) => r -> (c => r) -> r
maybeC d a = maybe d (\Dict -> a) $ optionalDict @c

tryC :: forall c r . (Hold c => c) => (c => r) -> Maybe r
tryC a = optionalDict @c <&> \Dict -> a
```

And indeed they would work

```haskell
class Foo where
instance Foo where

class Bar where

main :: IO ()
main = do
  print $ isSatisfied @Foo
  print $ isSatisfied @Bar

  print $ tryC @(Show Bool) $ show True
  print $ tryC @(Show (Int -> Int)) $ show $ id @Int
```

outputs

```haskell
True
False
Just "True"
Nothing
```

However there is a serious problem with this approach: since `Hold` is non-`{-# SOURCE #-}` imported,
`Hold c => c` constraint is trivial and solving as soon as possible. That means that we cannot define a
new function using `optionalDict` - its constraint would be immediately solved as trivial as we saw
before.

For example, we cannot move helpers to the `Main` module or define function `tryShow` to abstract
pattern in the code above - such functions would never get an optional instance.

# Delaying solving of `Hold c => c` constraint

If we stop for a minute now and think about possible semantic of optional constraints, we would see
two different possibilities here:

- Optional constraints may be solved only when constraint is fully instantiated with concrete types,
  like `Typeable`, so we can accurate judge about its satisfibility. But that means that `c` no
  longer implies `Optionally c`, which is kind of strange.

- Optional constraints may be solved immediately if not written explicitly in type signatures,
  marking constraint as unsatisfied if it is in current form.

I would probably prefer the former, because eager solving of optional constraints "breaks" type
inference: if we would write `foo = bar`, `foo` may behave differently than `bar` which I really
want to avoid. So we would delay solving of `Hold c => c` constraints as long as possible, until `c`
is fully instantiated.

First, let me define a synonym for it

```haskell
class (Hold c => c) => Optionally c where
instance (Hold c => c) => Optionally c where
```

Now we just need a way to prevent `Optionally` synonym from simplifying to `Hold c => c` as long as
possible. Sounds familiar to you? That's exactly what [opaque constraint
synonyms](https://blog.csongor.co.uk/opaque-constraint-synonyms/) trick does!

The trick is to introduce an overlapping instance for `Optionally`

```haskell
class Dummy where
instance {-# OVERLAPPING #-} (Hold Dummy => Dummy) => Optionally Dummy where
```

> __NOTE__ If you prefer the second semantic out of two mentioned above just replace all occurences of
> `OVERLAPPING` with `INCOHERENT`

Now ghc cannot simplify `Optionally c` to `Hold c => c`, because until `c` is instantiated, ghc does not know
which instance should it choose, even though they are completely equivalent.

But in our case it is not enough: this instance prevents only `Optionally c` from being solved.
Something like `Optionally (Show a)` would be expanded as `Show a` does not overlap with `Dummy`.

Instead we need something like this

```haskell
data family Any :: k

instance {-# OVERLAPPING #-} (Hold (f (g Any)) => f (g Any)) => Optionally (f (g Any)) where
instance {-# OVERLAPPING #-} (Hold (f (g Any a)) => f (g Any)) => Optionally (f (g Any a)) where
...

instance {-# OVERLAPPING #-} (Hold (f (g Any) z) => f (g Any) z) => Optionally (f (g Any) z) where
instance {-# OVERLAPPING #-} (Hold (f (g Any a) z) => f (g Any a) z) => Optionally (f (g Any a) z) where
...
```

Now those instances make constraints like `Optionaly (Show a)`, `Optionally (Show [a])`,
`Optionally (MonadReader r m)` ambiguous.

I hope nobody uses classes or types with more than 10 type parameters, so I've just generated 100
such instances with `CPP`

After we update `optionalDict` and helpers to use `Optionally`, we can easily write functions like

```haskell
tryShow :: forall a . Optionally (Show a) => a -> Maybe String
tryShow a = tryC @(Show a) $ show a
```

in `Main` and the resolution of `Optionally (Show a)` would be delayed until `a` would be instantiated with
some concrete type, so `tryShow` can be used like this

```haskell
main :: IO ()
main = do
  print $ tryShow True
  print $ tryShow $ id @Int
```

to get

```haskell
Just "True"
Nothing
```

Now `Optionally` constraints aren't solved until fully instantiated, which is in my opinion good
default, but solving them eagerly can be useful too, so let's just provide functions to give or
discard `Optionally` constraint manually

```haskell
newtype GiftQ c d a = GiftQ { unGiftQ :: (c => d) => a }

give :: forall c r . c => (Optionally c => r) -> r
give f = unGiftQ @(Hold c) @c $ unsafeCoerce (Gift @(Optionally c) f)

discard :: forall c r . (Optionally c => r) -> r
discard f = unGiftQ @(Hold c) @c $ unsafeCoerce (Gift @(Optionally c) f)
```

> __NOTE__ This way we could also define `resolve` to turn `Hold c => c` into `Optionally c`, but
> `resolve` would have an unpredictible semantic, e.g. `Show a` wouldn't imply `Optionally (Show [a])`,
> so I omit it here.

# Dangers of `Hold c => c`

Playing with this I found some interesting thing: how do you think, what would ghc say if we would
write incorrect version of `tryShow` like that

```haskell
tryShow :: forall a . Optionally (Show a) => a -> String
tryShow = show
```

You would probably expect this function to give `Could not deduce (Show a) ...` error, but this code
actually typechecks and with `main` above prints

```haskell
"True"
optionally-example: <<loop>>
```

I was pretty confused by this but after some struggling I found that given constraint `Hold c => c`
ghc can easily derive `c`! Using that trick we can "prove" absolutely anything with code like

```haskell
class c => Hold c where
instance c => Hold c where

data Dict c = c => Dict

anythingDict :: Dict c
anythingDict = go
  where
    go :: (Hold c => c) => Dict c
    go = Dict
```

Of course, it is impossible to get a dictionary for any class out of nothing so generated core simply
loops

```haskell
Rec {
$dHold_rxi :: forall {c :: Constraint}. Hold c
$dHold_rxi = $dHold_rxi
end Rec }

anythingDict :: forall (c :: Constraint). Dict c
anythingDict = \ (@c) -> Dict ($dHold_rxi `cast` <Co:2>)
```

That is in fact [a bug](https://gitlab.haskell.org/ghc/ghc/-/issues/19690): ghc isn't supposed to
generate bottom dictionaries but with `UndecidableSuperClasses` and `QuantifiedConstraints` it's
possible to get one.

Luckily in our case we can easily workaround this. The constraint `Hold c => c` is available in
`tryShow` as superclass of `Optionally c`, but we don't really need it to be one. Instead we can
make `Optionally` hold `Dict` of that superclass.

```haskell
data HoldDict c = (Hold c => c) => HoldDict

class Optionally c where
  optionallyHoldDict :: HoldDict c
```

With instances of `Optionally`, `optionalDict` and other functions changed to match these changes,
incorrect version of `tryShow` would be rejected with `Could not deduce (Show a)` but written
correct, everything would work as it used to.

# Limitations

I don't think this trick should be ever used in practice because it has a lot of problems:

- It relies heavily on ghc's unspecified behaviour, and while representation of instances as
  dictionaries is in my opinion reliable enough, the fact that ghc prefers global dictionaries to
  local one isn't.

- Without optimisations ghc wouldn't solve constraints that way so the whole trick works only with
  `-O`. It wouldn't work in ghci without `-fobject-code` enabled for example.

- It can break on newtype represented classes if bottom is stored as a method, as it was said in the
  "manipulating constraint" section. In that example, `isSatisfied @Foo` would result in exception
  thrown.

  ```haskell
  class Foo where foo :: ()
  instance Foo where foo = undefined
  ```

- It doesn't work with magic classes like `Typeable`, `KnownSymbol` or `~`.


- It gives some performance overhead: even if we would inline `optionalDict` (which I'm not sure is
  safe), it uses `unsafePerformIO` and `unsafeCoerce` that would prevent another optimisations.

- It breaks the open world assumption: if there exists an unimported orphan instance for `c`, it would
  not be detected by `optionalDict`. This seems to be a general problem of optional constraints
  rather than this concrete approach through.

- And probably some another problems I'm not aware of yet.

# Conclusion

> I think this all is as awful as fun. Awfun.
>
> effectfully, [automatically detecting and instantiating
> polymorphism](https://github.com/effectfully-ou/sketches/tree/master/poly-type-of-saga/part1-try-unify)

This quote perfectly describes how do I feel about this trick. It is elegant and beautiful
in some way, but at the same time it's terrible.

Still taking this as a challenge I really enjoyed making it work, and hope you enjoyed reading
about it, despite my writing being a mess.

Final code as well as some usage examples can be found at [github](https://github.com/aadaa-fgtaa/optionally).
