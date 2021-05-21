---
title: Brief introduction to Conway Game of Life with Comonads
date: 2021-04-29 22:00 +02:00
tags: [conway, game of life, cellular, automata, category theory, scala, haskell, functor, comonad, store]
description: Look how cellular automata looks like using category theory tools in FP languages
---

In this article we take a look at Conway's Game of Life as a good example for
comonad usage. We will go step-by-step through basic concepts finally presenting
an example of Conway's Game of Life implementation.

## Background

Functor is a polymorphic type that is able to wrap values in it. These values can
be mapped over using some function. This function is often called `map` or `fmap`, depending
on language's convention. In Haskell, this kind of type belongs to `Functor` type class.

```haskell
class Functor f where
  fmap :: (a -> b) -> f a -> f b
```

The simplest example you have probably dealt with is a list. List of values can be simply
mapped over by iterating over them, applying function to it and packing them back
to the list (to immutable copy or modifying existing list). 

Also well known case is
`Maybe` functor.
It allows to do null-safe computations
without tedious checking of null references with control flow statements. Also
there is richer functor called `Either` that allows to return a failure with
error details attached (`Left`) or can wrap a value coming from successful computation (`Right`).

We can say that functor gives value (or values) a *context* that attaches
specific behavior to it.

<!-- ### Monad

Very briefly, monad is a model of computation that is equipped with a functor `m`
and functions:
* `return :: a -> m a` that allows to inject bare value into the context;
* `bind :: (a -> m b) -> m a -> m b` that allows to run an action on wrapped value.
What does it mean to us in simple words? 

`return` function is
easy one, it just injects a value into the functor. For example,
in Haskell `return` function for `Maybe` can look like this:

```haskell
return :: a -> Maybe a
return a = Just a
```

It just takes the value and wraps it around `Maybe` using `Just` constructor.
Similarly we can write it for `Either` functor:

```haskell
return :: a -> Either e a
return a = Right a
```

`bind` function (called also as `flatMap` in other languages) allows to
map context-aware value (`m a`) with function (`a -> m b`) that 
returns a new context for it (`m b`). This allows to transform
our context-aware values in sequentional manner. That's way we have 
`for`-comprehensions in Scala or `do`-notation in Haskell and they are
using `bind` function to preform pure sequentional interface for computations.

In addition, we have function `join :: m (m a) -> m a` that brings functor layers
into one and it can be implemented using `bind` by saying `join = bind id`.
It is sometimes useful but it will be important in the next section. -->

### Comonads

Comonads, as name suggests, is a monad, but in an opposite category. 
By definition, if we have function `a -> b`, then in opposite category it would be
`b -> a`. So, let's rephrase `Monad` type class into `Comonad` type class using
this play:

```haskell
class Functor m => Monad m where
  return :: a -> m a
  join :: m (m a) -> m a
  bind :: (a -> m b) -> m a -> m b

class Functor w => Comonad w where
  coreturn :: w a -> a
  cojoin :: w a -> w (w a)
  cobind :: (w a -> b) -> w a -> w b
```

Comonads can be thought as a universe of values that has one distinguished point.
To get that point we can call `coreturn`.

`cobind` corresponds to `bind` function and it works like this.
`cobind` takes the universe and for each point creates an instance
of comonad that looks at this point. Then `cobind` calls function of type `w a -> b` 
on these copies, resulting a new universe that is made of `b`-s. 
`cojoin` does exactly same thing, but only provides these copies of universes.

You can see that you need implement `coreturn` and either `cobind` or `cojoin` to get the instance of `Comonad`.

For the rest of article we will call `coreturn` as `extract`, `cobind` as `extend` and 
`cojoin` as `duplicate`.

### `Store` comonad

So let consider a pair of values: an index of type `s` 
and function `s -> a` that accesses early mentioned
universe of values of type `a`. This is `Store` comonad.

```haskell
data Store s a = Store s (s -> a)
```

Shall we implement `Comonad` type class? Let's try implement `extract`.
```haskell
extract :: Store s a -> a
extract (Store idx get) = get idx

duplicate :: (Store s a -> b) -> Store s a -> Store s b
duplicate f (Store idx get) = Store idx (\i -> f (Store i get))
```
You can compare this implementation with the explanation of `extend` above.

## Game of Life v. `Store`

Game of Life, first of all, is a cellular automata. It has
an infinite discrete plane filled with two kind of cells --- dead or alive.
Each cell in each step is transformed by provided rules, and these rules are:
* if current cell is dead and has three alive neighbours, then cell becomes alive;
* if current cell is alive and has two or three alive neighbours, then cell still is alive;
* otherwise cell becomes dead.

We can start implementing this automaton by describing the board.

```haskell
type Coord = (Int, Int)
type CellPlane a = Store Coord a
data Conway = Dead | Alive deriving (Eq)
```

### Game Of Life is an `experiment`...

We need to obtain the state of neighbouring cells around the focused cell. We can implement function
called `experiment` in this way:

```haskell
experiment :: Functor f => (s -> f s) -> Store s a -> f a
experiment f (Store idx get) = fmap get (f idx)
```

This function passes the current index into the function `s -> f s` resulting
with `f s`. Since `f` is a functor, we can map `f s` with `get`.

If we'll consider a list as our functor `f`, this gives as a way to get neighbourhood.
```haskell
neighboursOf :: Coord -> [Coord]
neighboursOf (x, y) = [ (x + d, y + d') | d <- [-1..1], d' <- [-1..1], (d, d') /= (0, 0) ]

neighbouringCells :: CellPlane Conway -> [Conway]
neighbouringCells = experiment neighboursOf
```

### Game Of Life _rules_

So now we can easily write the rules of Game Of Life.

```haskell
golStep :: CellPlane Conway -> Conway
golStep p = case extract p of
  Dead  | noOfNeighbours == 3          -> Alive
  Alive | noOfNeighbours `elem` [2, 3] -> Alive
  _                                    -> Dead
 where
  -- @noOfNeighbours@ = number of neighbouring cells that are alive
  noOfNeighbours = length (filter (== Alive) (neighbouringCells p))
```

Now we need to embed an initial state (given by `Map Coord Conway`) into the comonad...
```haskell
toCellPlane :: Map Coord Conway -> CellPlane Conway
toCellPlane cs = State (0, 0) access
 where
  -- If we are out of bounds we assume that cell is dead.
  access (x, y) = fromMaybe Dead (Map.lookup (x, y) cs)
```
...as well as we want to extract a finite view of the cell plane.
```haskell
data Rect = Rect { x :: Int
                 , y :: Int
                 , w :: Int
                 , h :: Int
                 }
fromCellPlane :: CellPlane Conway -> Rect -> Map Coord Conway
fromCellPlane p (Rect x y w h) = Map.fromList (coords `zip` experiment (const coords) p)
 where
  coords = [(i, j) | i <- [x .. x + w - 1], j <- [y .. y + h - 1]]
```

Finally we can create an inifite list of evolutions using `iterate` function,
that repeatedly calls provided function to it (in our case it's `extend golStep`):
```haskell
evolutions :: CellPlane Conway -> [CellPlane Conway]
evolutions = iterate (extend golStep)
```

With these three functions --- `from/toCellPlane` and `evolutions` --- we can implement
a program that runs Game of Life.

## Summary

Undoubtly, this is an interesting way to describe various cellular automata, using ideas that comes
from category theory. Also comonads provides a way to generalise cellular automata to other spaces
than just infinite plane.

Only disadvantage of this solution is an exponential growth of running time, because at each `extend`ing the 
state of the plane is computed from scratch and memoising these computations 
could be useful. This can be easily achieved in 
Haskell using [trie-based pure memoisers](https://hackage.haskell.org/package/MemoTrie) or in Scala using a mutable `HashMap` to memoise partial results.