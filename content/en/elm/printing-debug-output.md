---
title:                "Elm recipe: Printing debug output"
simple_title:         "Printing debug output"
programming_language: "Elm"
category:             "Elm"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/printing-debug-output.md"
---

{{< edit_this_page >}}

## Why

Debugging is an essential part of the development process. It allows us to identify and fix errors in our code. One of the most common ways to debug in Elm is by using print statements to output information about our program at various points during execution. In this blog post, we will explore the importance of printing debug output and how it can help improve our development workflow.

## How To

To print debug output in Elm, we can use the `Debug.log` function. It takes in a string as the first argument, which is used as a label for the information we want to print. The second argument can be any value, and its string representation will be printed to the console.

```
Elm Debug.log example:

msg : String
msg = "Hello, World!"

greeting : String
greeting = Debug.log "Printing greeting" msg

-- Output:
-- Printing greeting: "Hello, World!"
```

This is a simple example, but it showcases how we can use the `Debug.log` function to print out information about our program. We can use this function in various situations, such as checking the value of a variable, tracking the execution flow, or identifying where an error occurs.

Another useful function for debugging is `Debug.toString`, which takes in a value and returns its string representation. We can use this function with the `Debug.log` function to print out the contents of a record or list.

```
Elm Debug.toString example:

user : { name : String, age : Int }
user = { name = "John", age = 28 }

userInfo : String
userInfo = Debug.log "Printing user info" (Debug.toString user)

-- Output:
-- Printing user info: "{ name = "John", age = 28 }"
```

## Deep Dive

One of the benefits of using print statements for debugging in Elm is that we can view the output in our browser's console. This allows us to quickly see the values and information we are printing without interrupting our program's execution.

Another advantage of using print statements is that they can be easily removed once we have fixed the issue or identified the desired information. This means we don't have to clutter our code with unnecessary debugging code.

However, a potential downside of using print statements is that they can make our code less readable if we don't remove them before deploying our application. Therefore, it is essential to use them strategically and remove them once we have finished debugging.

## See Also
- [Debugging Elm Apps](https://guide.elm-lang.org/debugging/)
- [Logging in Elm](https://package.elm-lang.org/packages/elm/log/latest/)
- [Using Elm's Debug Module](https://www.elm-tutorial.org/en/04-starting/06-debugger.html)