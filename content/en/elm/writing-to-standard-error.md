---
title:                "Elm recipe: Writing to standard error"
simple_title:         "Writing to standard error"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Why
Writing to standard error is an important aspect of Elm programming. This feature allows developers to send error messages and debugging information to the terminal, making it easier to identify and resolve issues in their code.

## How To
To write to standard error in Elm, we can use the `Debug.log` function. Let's say we want to log a message every time a certain function is called. We can use the `Debug.log` function like this:

```Elm
import Debug exposing (log)

func: Int -> Int
func x = 
    let
        result = x * 2
    in
        log ("Func called with input: " ++ toString x) result
```
In this example, we have imported the `Debug` module and used the `log` function to log a message before returning the result of the function. This will print the message and the result of the function to the terminal whenever the function is called.

We can also use `Debug.log` to log values at different points in our code, helping us track the flow of data. For example:

```Elm
import Debug exposing (log)

func: Int -> String
func x =
    let
        doubled = x * 2
        uppercased = String.toUpper (toString doubled)
    in
        log "Value before uppercasing:" doubled
        log "Final value:" uppercased
        uppercased
```
This will log the value of `doubled` before and after it is uppercased, giving us a better understanding of how our code is working.

## Deep Dive
Behind the scenes, `Debug.log` is using a special `Cmd` type to send the message to the terminal. When we call `Debug.log` with a message and a value, it creates a `Cmd` with that message and value, and then sends it to the debugger. This `Cmd` is then executed, printing the message and value to the terminal.

It's important to note that `Debug.log` should only be used for debugging purposes and should be removed or replaced with proper error handling in production code. Using it too much can also slow down the performance of our code.

## See Also
- Official Elm documentation on `Debug.log`: https://package.elm-lang.org/packages/elm/core/latest/Debug#log
- Blog post on debugging in Elm: https://dev.to/servo118/debugging-in-elm-2ko8