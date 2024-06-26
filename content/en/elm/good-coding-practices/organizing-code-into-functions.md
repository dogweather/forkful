---
date: 2024-01-25 02:59:38.101508-07:00
description: 'How to: Here''s a chunk of Elm code with a simple function to greet
  a user.'
lastmod: '2024-03-13T22:45:00.015653-06:00'
model: gpt-4-1106-preview
summary: Here's a chunk of Elm code with a simple function to greet a user.
title: Organizing code into functions
weight: 18
---

## How to:
Here's a chunk of Elm code with a simple function to greet a user:

```Elm
module Main exposing (..)

import Html exposing (text)

greetUser : String -> String
greetUser userName =
    "Hello, " ++ userName ++ "!"

main =
    text (greetUser "Casey")
```

Run it, and you'll get the output: "Hello, Casey!"

Now, let's say you want to add more personalization. Extract more functionality!

```Elm
module Main exposing (..)

import Html exposing (text)

greetUser : String -> String -> String
greetUser greeting userName =
    greeting ++ ", " ++ userName ++ "!"

personalGreeting : String -> String
personalGreeting userName =
    greetUser "Howdy" userName

main =
    text (personalGreeting "Casey")
```

Now, when you run it: "Howdy, Casey!" Magic? Nope, just functions doing their thing.

## Deep Dive
Back in the day, code was often one long sequence of instructions (think spaghetti code). It was a nightmare to maintain. Then structured programming came along, and with it, functions. Elm, like its functional programming predecessors, relies heavily on functions for organization. 

You can nest functions, creating closures, or keep them pure for simplicity. Elm encourages the latter: pure functions with well-defined inputs and outputs, leading to easier debugging and testing. 

Elm functions can also be higher-order, meaning they can accept or return other functions. This opens up a world of composability. However, unlike some other languages, Elm doesn't have function overloading; each function must have a unique name.

Additionally, Elm imposes a strong static typing system that not only checks the types but also infers them, reducing the boilerplate code.

When compared to alternatives like procedural or object-oriented code organization in other languages, Elm's approach emphasizes simplicity and predictability. Elm doesn't have objects or classes. You organize code with functions and modules instead of classes and instances.

## See Also
To dig deeper, check out these resources:
- Elm's official guide on functions: https://guide.elm-lang.org/core_language.html
- Elm package documentation for more complex function examples: https://package.elm-lang.org/
- Learn about Elm's type system, which plays nicely with function organization: https://elm-lang.org/docs/types
