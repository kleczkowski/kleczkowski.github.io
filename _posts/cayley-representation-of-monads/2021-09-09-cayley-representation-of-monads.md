---
title: Cayley Representation of... Monads?
date: 2021-09-09 16:20:00 +01:00
tags: [haskell, category theory, monads, codensity, free, list, reverse, type theory]
description: Can we find Cayley representation of a monad?
---

In Haskell we're using monoids intensively - monoid is an algebraic structure
that has associative binary operation with neutral element. 

```haskell

class Monoid a where
  mempty :: a 
  mappend :: a -> a -> a

```

For instance,
we have lists that form monoid under list concatenation and empty list
as neutral element.

## Short Tale About Lists

You're a programmer in some software house that creates embeddable software
for intelligent wash machines --- in Haskell. Your task is to create
`reverse` function that reverses the list, for example:

```haskell

reverse [] = []
reverse [3, 2, 1] = [1, 2, 3]
reverse [2, 2, 3, 3] = [3, 3, 2, 2]

```

If you know Haskell in essential extent, you just write function like this:

```haskell

reverse :: [a] -> [a]
reverse [] = []
reverse (a : as) = reverse as ++ [a]

```

You committed the code and you're using it every day in your software, but
there is a problem. Your function is pretty slow on long lists.

Why? Let's look at list concatenation.

```haskell

(++) :: [a] -> [a] -> [a]
[] ++ bs = bs
(a : as) ++ bs = a : (as ++ bs)

```

Calling reverse on `[1, 2, 3, 4, 5]` will create following thunk:

```haskell

reverse [1, 2, 3, 4, 5] 
  = reverse [2, 3, 4, 5] ++ [1]
  = (reverse [3, 4, 5] ++ [2]) ++ [3]
  = ((reverse [4, 5] ++ [3]) ++ [2]) ++ [1]
  = ...
  = (((([] ++ [5]) ++ [4]) ++ [3]) ++ [2]) ++ [1]

```

Each call of `(++)` at `n`-th number takes `5 - n + 1` steps to
concatenate `[n]` list. So `reverse` is `O(n^2)`. Not good.

What if we can encode each action as a function that operates on list?
We call these function as differential list, because we encode only changes in list
rather than all content in one place.

```haskell

type DList a = [a] -> [a]

empty :: DList a
empty = id

singleton :: a -> DList a
singleton a = (a :)

concat :: DList a -> DList a -> DList a
concat f g = f . g

toList :: DList a -> a
toList f = f []

```

Now we can implement `reverse` using `DList`.

```haskell

reverse :: [a] -> [a]
reverse = toList . go
  where
    go :: [a] -> DList a
    go [] = empty
    go (a : as) = go as `concat` singleton a

```

We replaced `(++)` with basically `(.)` so concatenation time
(with left associativity) is constant in comparison to lists' lengths.
This brings significant runtime improvement that we wanted to achieve.

`DList` is Cayley representation of list monoid. What does that mean?
Cayley's theorem says that every monoid `M` is isomorphic to the submonoid of automorphisms `M -> M`.

For sure `DList` fits in the Cayley's theorem.

The moral of this story is that sometimes operation of "original" monoid
can be not effective enough, so we can switch to the Cayley representation
that is more preformant. Also you can notice that during building complex object
using Cayley's representation we can't "peek" our object during building process.
So this is kind of trade-off using this technique.

## Cayley for Monads

Now we can see that Cayley's theorem not only applies for classic monoids. We can
consider, of course, monads, because monad is a monoid in category of endofunctors
under functor composition.

```haskell

class Functor m => Monad m where
  return :: a -> m a
  (>>=)  :: m a -> (a -> m b) -> m b

```

But this is not easy task since this category notably differs from `Hask` category.
This category is called `[Hask, Hask]`. Our goal will be deriving the exponential object
(every exponential in `Hask` is a function). Then having monad `M` we'll use `M^M` as
Cayley's representation of monad `M`.

We know that every function can be curried and uncurried, this forms one of famous adjunctions,
that can be described as `Hom(a * b, c) ~= Hom(a, c^b)`.

Also we know that every `Hask` functor can be represented using Yoneda lemma, that is,
functor instance `FA` is isomorphic to the natural transformation from functor
`Hask(A, -)` to functor `F`.

Now we can connect these two facts to derive functor `M^N A`.

```console

M^N A ~= Nat(Hask(A, -), M^N)       # by Yoneda lemma
      ~= Nat(Hask(A, -) `o` N, M)   # by "curry" adjunction, `o` means composition
   
```

So using this we can define `Exp` data type:

```haskell

newtype Exp m n a = Exp { appExp :: forall r. (a -> n r) -> m r }

```

(Natural transformations are `type f ~> g = forall r. f r -> g r` in Haskell).

A functor `M^M` is a codensity monad, widely used with free monads.
Since free monads are just like lists in runtime, but more restrictive
by design, using Cayley's representation on it can improve time spent on creating a list of actions of this free monad. This concludes the article.