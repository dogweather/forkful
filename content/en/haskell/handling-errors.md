---
title:                "Handling errors"
date:                  2024-01-21T21:19:30.352445-07:00
model:                 gpt-4-1106-preview
simple_title:         "Handling errors"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/handling-errors.md"
---

{{< edit_this_page >}}

## What & Why?
Handling errors in programming is about managing the unexpected—things that can go wrong. Programmers do it to ensure that their programs can cope with these situations gracefully, without crashing or producing wrong results.

## How to:
Haskell handles errors robustly through types like `Maybe` and `Either`. Here's a quick look:

```Haskell
safeDivide :: Integral a => a -> a -> Maybe a
safeDivide _ 0 = Nothing  -- Dividing by zero is a no-go, so we return Nothing.
safeDivide x y = Just (x `div` y)  -- Otherwise, we're all good, return the result in a Just.

-- Let's see it in action:
example1 :: Maybe Int
example1 = safeDivide 10 2  -- Just 5

example2 :: Maybe Int
example2 = safeDivide 10 0  -- Nothing
```

For more complex error handling, `Either` comes into play:

```Haskell
safeDivideEither :: Integral a => a -> a -> Either String a
safeDivideEither _ 0 = Left "Divide by zero error."  -- This time, the error carries a message.
safeDivideEither x y = Right (x `div` y)

-- And in use:
example3 :: Either String Int
example3 = safeDivideEither 10 2  -- Right 5

example4 :: Either String Int
example4 = safeDivideEither 10 0  -- Left "Divide by zero error."
```

## Deep Dive
In the Haskell world, error handling has a strong history. Back in the day, errors could bring your whole program down—no fun. Haskell's type system offers ways to make this a lot less likely. We've got `Maybe` and `Either`, but there are others like `Exceptions` and `IO` for different scenarios.

`Maybe` is simple: you get `Just` something if all's well, or `Nothing` if it's not. `Either` steps it up, allowing you to return an error message (`Left`) or a successful result (`Right`).

Both are pure, meaning they don't mess with the outside world – a big deal in Haskell. We avoid the pitfalls of unchecked exceptions that plague some other languages.

For those not content with `Maybe` and `Either`, libraries like `Control.Exception` provide more traditional, imperative-style error handling through exceptions. But using them too liberally can complicate things, so the community often sticks to the types.

## See Also
Dive deeper with:

- Haskell's own docs: [Haskell](https://haskell.org/documentation)
- Great for beginners: ["Learn You a Haskell for Great Good!"](http://learnyouahaskell.com/)
- For a deep dive into types and error handling: [Haskell Wiki](https://wiki.haskell.org/Error_handling)