---
title: Actor Model in Haskell
date: 2020-09-30 16:40:00 +02:00
tags: [haskell, actor model, concurrency, short article]
description: Learn how we can achive simple actor model using Haskell
usemathjax: true
---

We'll take a look at very popular way to describe concurrent computations.
Actor model is mainly based on $$\pi$$-calculus, well known theoretical model
developed by R. Milner.

Main principle of this model is _everything is an actor_. So what could actor do?

1. Actor can send message to other actor.
2. Actor can receive message from other actor.
3. Actor can designate next behaviour to be used for the next message it receives.
4. Actor can spawn finite number of actors. That is, actor model is hierarchical.

In this short article we'll focus on simple implementation of actor model in Haskell using
software transaction memory.

## What does an actor consist of?

Each actor is basically a concurrent job that consist of message queue. 
Actor pulls message from queue to do some action, especially to send or receive 
other messages, spawn an actor or change his behaviour.

To implement this queue we can use software transaction memory to do this safely.
STM gives us a way of controlling memory using transactions. 

Transaction principle says us if we execute multiple instructions using transaction
either none or each will be preformed to do a desirable efect (e.g. enqueuing multiple messages).
STM also ensures us that no one modifies target state, so there's no possibility
to read corrupted state.

We'll use `TQueue` from `stm` package to use it as data structure that allows to enqueue messages.

So we can start with our module preamble:

```haskell
module Control.Concurrent.Actor
  ( ActorRef (refId)
  , Behaviour (..)
  , spawn
  , send
  )
where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
```

Then we write a definition of actor reference.

```haskell
data ActorRef msg
  = ActorRef
  { refId   :: ThreadId
  , refMbox :: TQueue msg
  } deriving (Eq)
```

## Spawning an actor

To spawn an actor we need to instantiate `TQueue` and create a new green thread
using `forkIO`. Also we need to think, how we can model changing behaviour of actor.

We assume that after receiving a message actor wants to designate a new (or the same)
closure that can handle new messages.

Consider this datatype:

```haskell
newtype Behaviour msg = Behaviour { getBehaviour :: msg -> IO (Behaviour msg) }
```

So we can write a loop that can executes a sequence of generated `Behaviour`s
by executing `IO` actions.

```haskell
spawn :: Behaviour msg -> IO (ActorRef msg)
spawn b0 = do
  mbox <- newTQueueIO
  let go (Behaviour b) = void $ do
    msg <- atomically (readTQueue mbox)
    b msg >>= go
  pid  <- forkIO (go b0)
  pure (ActorRef pid mbox)
```

## Communication with actors

We implemented message receiving by switching behaviours. Now we want to do
sending routine that will allow to trigger other actors.

That is simple --- we need to enqueue message into another mailbox.

```haskell
send :: ActorRef msg -> msg -> IO ()
send msg recipent = atomically (writeTQueue (refMbox recipent) msg)
```

## Examples 

We can implement a file reader using actor model by defining his behaviour 
and set of messages to communicate with him.

```haskell
{-# LANGUAGE LambdaCases #-}
import Data.IORef
import System.IO

data FileReaderMsg
  = OpenFile FilePath
  | SendLine (ActorRef FileReaderMsg)
  | GotLine String
  | CloseFile
  deriving (Eq, Show)

fileReader :: Behaviour FileReaderMsg
fileReader = whenClosed
  where
    whenClosed = Behaviour $ \case
      OpenFile fp -> do
        h <- openFile fp ReadMode
        pure (whenOpened h)
      CloseFile -> pure whenClosed -- does nothing
      _ -> error "inappropiate state"
    whenOpened h = Behaviour $ \case
      SendLine replyTo -> do
        line <- hGetLine h
        replyTo `send` GotLine line
        pure (whenOpened h)
      CloseFile -> do
        hClose h
        pure whenClosed
      _ -> error "inappropiate state"
```

Then we can use `spawn` to get the instance of actor pointed by `ActorRef`.

# Summary

This model is very popular among other programming languages such as Scala or C#, because it allows to decouple
all system parts using transparent way to communicate between them. 

As usual, with Haskell we can get simple ideas working with simple implementations. Of course, this is only
the taste how actor-based models could look like. The way to improve this implementation is tightening `Behaviour`
type to eliminate or mitigate _partialness_ of behaviours.
