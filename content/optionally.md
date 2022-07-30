+++
title="Checking For Uncheckable: Optional Constraints"
date=2021-04-17
updated=2022-07-30

[taxonomies]
tags = ["haskell"]
+++

# The challenge

Is there a way to check if a constraint is satisfied and acquire its dictionary if it is without forcing the user to
define boilerplate instances by hand, like [constraint-unions](https://github.com/rampion/constraint-unions), or use
template haskell to generate them, like [ifctx](https://github.com/mikeizbicki/ifcxt)?

In other words, is there a way to define

```haskell
class Optionally c where
  optionalDict :: Maybe (Dict c)
```

such that `Optionally c` would always be satisfied for any concrete `c`, like `Typeable`, and `optionalDict` would be
`Just Dict` if constraint `c` is satisfied at call side, and `Nothing` otherwise?

I found a solution that I think is interesting and in some sense beautiful (and at the same time an ugly hack).

> __DISCLAIMER__ This trick definitely shouldn't be used for anything serious, it's just an abuse of ghc unspecified
> behaviour. Don't repeat this at home!

> __NOTE__ To understand this post you would probably need some familiarity with ghc core, especially with the
> representation of type classes and instances. [This talk](https://www.youtube.com/watch?v=fty9QL4aSRc) by Vladislav
> Zavialov may be a good introduction.

# Constant dictionaries optimization

As you probably know, ghc is usually very "stubborn" about constraints: once a constraint is introduced as wanted, ghc
would either solve it or emit compilation error if it cannot be solved. This is for good, indeed: the behavior ensures
that instances defined in other modules would never change current module's behavior.

This makes checking for instance's existence very difficult. One possible solution is to use `OVERLAPPING/INCOHERENT`
pragmas and define one instance of `Optionally` for each satisfied `c`

```haskell
instance {-# OVERLAPPING #-} Optionally c where
  optionalDict = Nothing

instance {-# OVERLAPS #-} Optionally (Show Int) where
  optionalDict = Just Dict

instance {-# OVERLAPS #-} Optionally (Show Bool) where
  optionalDict = Just Dict

...
```

That way we never introduce `c` as wanted constraint, but rather just pattern-match on it to determine if we have a
dictionary for it. However, this forces us to define an instance of `Optionally` for each fully instantiated `c`, which
is very boilerplatish. One option is to use template haskell to generate such instances in each module where we use
optional constraints, like [ifctx](https://github.com/mikeizbicki/ifcxt) does, but I wanted more user-friendly solution.


Despite ghc's "stubbornness", there are still some cases when ghc behaves differently depending on wanted constraint
being satisfied, so that these both cases don't cause compilation error. One of such cases is the constraint
dictionaries optimizations enabled by `-fsolve-constant-dicts` (or simply `-O`): when it is enabled, ghc prefers to solve
a constraint with a top-level dictionary rather than local one passed to a function.

For example in

```haskell
test :: Eq Integer => Dict (Eq Integer)
test = Dict
```

ghc with `-fsolve-constant-dicts` would just ignore passed constraint and use top-level `$fEqInteger` instead, producing
the following core

```haskell
test1 :: Dict (Eq Integer)
test1 = Dict $fEqInteger

test :: Eq Integer => Dict (Eq Integer)
test = \ _ -> test1
```

In case of unsatisfied constraint, like `Eq (Integer -> Integer)`, there is no top-level dictionary available, so ghc is
forced to use given dictionary, thus

```haskell
test :: Eq (Integer -> Integer) => Dict (Eq (Integer -> Integer))
test = Dict
```

would result in

```haskell
test :: Eq (Integer -> Integer) => Dict (Eq (Integer -> Integer))
test = Dict
```

(Ab)using this fact we can check if a constraint is satisfied by defining such `test` function, passing a bottom-valued
dictionary to it and checking if an exception occurs. If it did, the constraint is probably unsatisfied, otherwise we
will get a `Dict` of it.

Of course, making `optionalDict` accept `c => Dict c` explicitly would make api awful - every caller of function that
uses an optional constraint would need to pass `Dict` as an argument to it. Luckily, we can make this argument implicit
with help of `QuantifiedConstraints` - constraint solving for them would apply the same optimization if possible, for
example in

```haskell
test :: ((Eq Integer => Eq Integer) => r) -> r
test f = f
```

ghc would again ignore `Eq Integer` and pass `\_ -> $fEqInteger` to `f`

```haskell
test1 :: Eq Integer => Eq Integer
test1 = \ _ -> $fEqInteger

test :: forall r . ((Eq Integer => Eq Integer) => r) -> r
test = \ (@ r) (f :: (Eq Integer => Eq Integer) => r) -> f test1
```

Also note that despite its name `-fsolve-constant-dicts` isn't limited to using constant, i.e. top-level, dictionaries.
It would also use less-deeply bound dictionaries instead of more deep ones, for example in

```haskell
test :: Eq c => ((Eq c => Eq c) => r) -> r
test f = f
```

`Eq c` passed to `test` would be used to solve `Eq c => Eq c` required for `f`

```haskell
test :: forall c r . Eq c => ((Eq c => Eq c) => r) -> r
test = \ (@ c) (@ r) ($dEq :: Eq c) (f :: (Eq c => Eq c) => r) -> f (\ _ -> $dEq)
```

# Hiding the truth with .hs-boot files

Now we roughly understand that the definition of `Optionally c` should be something of form `g => c` where `c` can be
always solved using `g`. An obvious choice would be `c => c` but such constraint loops the typechecker leading to `too
many iterations` errors. The solution is to wrap given constraint into constraint-level newtype, `Hold`:

```haskell
class c => Hold c
instance c => Hold c
```

Now `Optionally` would now just a synonym for `Hold c => c`. When `-fsolve-constant-dicts` is active and there is a
dictionary for `c` in scope such constraint would be solved with `\_ -> cDict`, and `$p1Hold`, a selector that extracts
the first superclass of `Hold`, otherwise.

But adding such constraint to `optionalDict` would immediately reveal a simple problem: the constraint `Hold c => c` is
in fact redundant, thus `ghc` would never use dictionary that is passed to `optionalDict`. Instead it would use
top-level `$p1Hold` to solve all such constraint, completely ignoring that passed dictionary.

To illustrate, for

```haskell
useHold :: forall c . (Hold c => c) => Dict (Hold c) -> Dict c
useHold = sub @(Hold c) @c

sub :: (a => b) => Dict a -> Dict b
sub Dict = Dict
```

generated core would be

```haskell
useHold :: forall (c :: Constraint). (Hold c => c) => Dict (Hold c) -> Dict c
useHold = \ (@ c) _ (d :: Dict (Hold c)) -> case c of Dict i -> Dict (i `cast` <Co:2>)
```

Notice how the dictionary for `Hold c => c` in `useHold`'s definition is simply ignored. We really don't want this to
happen - after all, the whole idea is to distinguish satisfied constraints from unsatisfied ones by looking at the
passed dictionary for `Hold c => c`. At the moment we are always dealing with `$p1Hold`, which gives us no information
at all.

To prevent ghc from dropping the constraint we need to hide the fact that `c` is the superclass of `Hold c`, so that ghc
wouldn't be able to solve `Hold c => c` with `$p1Hold` at the definition of `Optionally`. However it should be available
at the call side as we need `Hold c => c` would always be satisfied there. Thus, the goal is to make ghc forget that `c`
is the supreclass of `Hold c` when defining `Optionally`, but leave that information available at call side.

I don't know any way to control exporting of superclasses (and doubt that one could exist), but there is another
~~terrible hack~~ option: instead of making ghc forget about `Hold`'s superclass we would make ghc not-yet-know about it
by putting forward declaration of `Hold` without its superclass into `.hs-boot` file.

The module defining `Optionally` would `{-# SOURCE #-}`-import `Hold` so that it would be an abstract class without any
information about superclasses available, whereas at the use side `Hold` would be imported normally and `Hold c => c`
would be trivial.

The whole thing looks something like that

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

class c => Hold c
instance c => Hold c
```

Now `useHold` uses `Hold c => c` constraint as expected:

```haskell
useHold :: forall (c :: Constraint) . (Hold c => c) => Dict (Hold c) -> Dict c
useHold = \ (@ c) (d :: Hold c => c) -> sub d
```

# unsafeCoerce 'em all

Now we need to check if passed constraint actually uses `Hold c` argument by passing a bottom-valued dictionary
to it and checking if an exception occurs. If it doesn't, we would get `Dict c` and return it, otherwise the constraint
is probably unsatisfied and we return `Nothing`.

In other words, we need `undefined` and `seq` but for constraints, i.e.

```haskell
errorDict :: Dict c
errorDict = Dict undefined

forceDict :: Dict c -> ()
forceDict (Dict d) = d `seq` ()
```

Of course, haskell doesn't let us manipulate constraints directly, but there is nothing a couple of `unsafeCoerce`s
couldn't do:

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

> __NOTE__ Some explanations of this trick can be found
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

> __NOTE__ In fact, there is a problem with this implementation of `forceDict`: if `c` is represented with newtype, e.g.
> is a single method class, `forceDict` would force that method instead of forcing the dictionary. It would be very bad
> if method is bottom or expensive to compute. I don't see any way to fix it so I would just hope that this case is
> corner enough to ignore.

# The rest of the owl

Now the definition of `optionalDict` is pretty straightforward

```haskell
optionalDict :: forall c . Optionally c => Maybe (Dict c)
optionalDict = unsafeDupablePerformIO $ catch
  do evaluate (forceDict c) $> Just c
  do \NoInstanceError -> pure Nothing
  where
    c :: Dict c
    c = sub @(Hold c) @c errorDict
-- I'm not sure if this NOINLINE is really needed, but there is 'errorDict' inside
-- so I want to be sure that ghc wouldn't pass that dictionary somewhere else.
{-# NOINLINE optionalDict #-}
```

But instead of working with `optionalDict` directly it is often simpler to use some combinators defined in terms of it,
e.g.

```haskell
isSatisfied :: forall c . Optionally c => Bool
isSatisfied = isJust $ optionalDict @c

maybeC :: forall c r . Optionally c => r -> (c => r) -> r
maybeC d a = maybe d (\Dict -> a) $ optionalDict @c

tryC :: forall c r . Optionally c => (c => r) -> Maybe r
tryC a = optionalDict @c <&> \Dict -> a
```

And indeed they would work:

```haskell
class Foo
instance Foo

class Bar

main :: IO ()
main = do
  print $ isSatisfied @Foo
  print $ isSatisfied @Bar

  print $ tryC @(Show Bool) $ show True
  print $ tryC @(Show (Int -> Int)) $ show $ id @Int
```

would print

```haskell
True
False
Just "True"
Nothing
```

# Late resolution for optional constraints

However there is a serious problem with the code as is: as `Hold` is non-`{-# SOURCE #-}` imported in user's code, `Hold
c => c` constraint is trivial and is solved as soon as possible, meaning there is no way to define a new function with
`Optionally` constraint - it would be solved immediately as trivial, and `optionalDict` would always be `Nothing`.

For example, we cannot move combinators defined above to the `Main` module. Likewise, there is no way to define `tryShow`
to abstract pattern in the code above - such functions would always return `Nothing`.

If we stop for a minute now and think about possible semantic of optional constraints, we would see two different
possibilities here:

In fact, there is two possible behaviors of optional constraints:

- Optional constraints may be solved only when constraint is fully instantiated with concrete types, like `Typeable`, so
  is is possible to judge accurately about its satisfiability. Sadly, that means that `c` no longer implies `Optionally
  c`, which is kind of weird.

- Optional constraints may be solved immediately if not written explicitly in type signatures, like `HasCallStack`,
  solving constraint as unsatisfied if it couldn't be solved in its current form.

Following the specification given at the beginning of this post, I would implement the first option, because eager
solving means that let-binding some subexpression can change behavior of the program, for example with `foo = bar`,
`foo` may behave differently than `bar` which I really want to avoid.

The goal is thus to delay solving of `Hold c => c` constraints until `c` is fully instantiated.

Let me begin with defining a constraint synonym for `Hold c => c`

```haskell
class (Hold c => c) => Optionally c where
instance (Hold c => c) => Optionally c where
```

Now we just need a way to prevent `Optionally` from reducing to `Hold c => c` as long as possible. Sounds familiar?
That's exactly what [opaque constraint synonyms](https://blog.csongor.co.uk/opaque-constraint-synonyms/) trick does!

The solution is to introduce an overlapping instance for `Optionally`

```haskell
class Dummy where
instance {-# OVERLAPPING #-} (Hold Dummy => Dummy) => Optionally Dummy where
```

> __NOTE__ It is possible to use `INCOHERENT` instead of `OVERLAPPING` here - that way we would have eager solving of
> optional constraint as described above.

Now ghc cannot reduce `Optionally c` to `Hold c => c` until `c` is fully instantiated, because ghc does not know which
instance should it choose (even though they are completely equivalent).

But in our case it is not enough: this instance prevents only `Optionally c` from being solved. Something like
`Optionally (Show a)` would be expanded as `Show a` does not overlap with `Dummy`.

Luckily, this can be solved with some more dummy instances like this

```haskell
data family Any :: k

instance {-# OVERLAPPING #-} (Hold (f (g Any)) => f (g Any)) => Optionally (f (g Any)) where
instance {-# OVERLAPPING #-} (Hold (f (g Any a)) => f (g Any)) => Optionally (f (g Any a)) where
...

instance {-# OVERLAPPING #-} (Hold (f (g Any) z) => f (g Any) z) => Optionally (f (g Any) z) where
instance {-# OVERLAPPING #-} (Hold (f (g Any a) z) => f (g Any a) z) => Optionally (f (g Any a) z) where
...
```

Those instances are enough to make complex constraints like `Optionaly (Show a)`, `Optionally (Show [a])`, `Optionally
(MonadReader r m)` ambiguous.

I really hope that nobody uses classes or types with more than 10 type parameters, so I've just generated 100 such
instances with CPP.

> __UPDATE__ Unfortunately, these instances are not enough to make constraints involving deeply nested types ambiguous,
> for example `Optionally (Show [[a]])` would be resolved immediately even with dummy instances above. To make such
> constraints ambiguous we need more dummy instances with deeper nesting of `Any`, like `Optionally (f (g1 (g2 Any)))`,
> `Optionally (f (g1 (g2 Any x)))`, etc.


After we update `optionalDict` and helpers to use `Optionally`, we can easily write functions like

```haskell
tryShow :: forall a . Optionally (Show a) => a -> Maybe String
tryShow a = tryC @(Show a) $ show a
```

in `Main` and the resolution of `Optionally (Show a)` would be delayed until `a` would be instantiated with some
concrete type, so `tryShow` can be used like this

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

Late resolution of optional constraints is in my opinion a good default, but solving them eagerly can be useful too, so
we can provide functions to give or discard `Optionally` constraint manually

```haskell
newtype GiftQ c d a = GiftQ { unGiftQ :: (c => d) => a }

give :: forall c r . c => (Optionally c => r) -> r
give f = unGiftQ @(Hold c) @c $ unsafeCoerce (Gift @(Optionally c) f)

discard :: forall c r . (Optionally c => r) -> r
discard f = unGiftQ @(Hold c) @c $ unsafeCoerce (Gift @(Optionally c) f)
```

> __NOTE__ This way we could also define `resolve` to turn `Hold c => c` into `Optionally c`, but it would have an
> unpredictible behaviour, e.g. `Show a` wouldn't imply `Optionally (Show [a])`, so I prefer to omit it here.

# Dangers of Hold c => c

Playing with the implementation, I found some interesting case: what do you think would ghc say if we would write
incorrect version of `tryShow` like this

```haskell
tryShow :: forall a . Optionally (Show a) => a -> String
tryShow = show
```

You probably would expect this function to give `Could not deduce (Show a) ...` error, but in fact this code typechecks
and with `main` above prints

```haskell
"True"
optionally-example: <<loop>>
```

This was really confusing but after some struggling I found out that given the constraint `Hold c => c` ghc willingly
derives `c`! Using that trick we can "prove" absolutely anything with code like

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

Of course, it is impossible to get a dictionary for any class out of nothing, so generated code simply loops:

```haskell
Rec {
$dHold_rxi :: forall {c :: Constraint}. Hold c
$dHold_rxi = $dHold_rxi
end Rec }

anythingDict :: forall (c :: Constraint). Dict c
anythingDict = \ (@c) -> Dict ($dHold_rxi `cast` <Co:2>)
```

That is in fact [a bug](https://gitlab.haskell.org/ghc/ghc/-/issues/19690) in ghc: it isn't supposed to produce a
bottom-valued dictionaries, yet with `UndecidableSuperClasses` and `QuantifiedConstraints` it's possible to trick ghc
into getting one.

Luckily in our case there exists a simple workaround: the problematic constraint `Hold c => c` is available in `tryShow`
as superclass of `Optionally c`, but it doesn't really have to be one. Instead, we would store that constraint wrapped
in `Dict` as a method of `Optionally`:

```haskell
data HoldDict c = (Hold c => c) => HoldDict

class Optionally c where
  optionallyHoldDict :: HoldDict c
```

Now to access `Hold c => c` constraint one should explicitly match on `optionallyHoldDict`s result, which is impossible
for user to do as it isn't exported. With everything updated to match these changes, incorrect version of `tryShow`
above would be rejected with `Could not deduce (Show a)` as expected, whereas the correct version would work as it
used to.

# Limitations

I don't think this trick should be ever used in practice due to the number of shortcomings:

- It relies heavily on ghc's `-fsolve-constant-dicts` optimizations. While representation of instances as dictionaries
  and reflection trick is in my opinion reliable enough, the fact that ghc prefers global dictionaries to local one
  isn't, and in fact the whole thing wouldn't work with `-O0` unless `-fsolve-constant-dicts` is explicitly enabled.

- It break on newtype-represented classes if bottom is stored as a method, as mentioned above. For example, with the
  code below `isSatisfied @Foo` would result in exception thrown instead of `True`.

```haskell
class Foo where foo :: ()
instance Foo where foo = undefined
```

- It doesn't work with `~`.

- It introduces some overhead: even if `optionalDict` would be inlined (which I'm not sure is safe), it uses
  `unsafeDupablePerformIO` and `unsafeCoerce` which would likely prevent further optimizations.

- It breaks the open world assumption: if there exists an unimported orphan instance for `c`, it would not be detected
  by `optionalDict`. This seems to be a general problem of optional constraints rather than this specific approach
  through.

- And probably some another problems I'm not aware of yet.

# Conclusion

> I think this all is as awful as fun. Awfun.
>
> @effectfully, [automatically detecting and instantiating
> polymorphism](https://github.com/effectfully-ou/sketches/tree/master/poly-type-of-saga/part1-try-unify)

This quote perfectly describes how do I feel about this trick: it is elegant and beautiful in some way, but at the same
time it's terrible.

Still, taking this as a challenge it was really interesting to make it work, and hope you enjoyed reading about it
despite my writing being a mess.

Final code as well as some usage examples can be found at [github](https://github.com/aadaa-fgtaa/optionally).
