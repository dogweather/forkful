---
title:                "Printing debug output"
html_title:           "Arduino recipe: Printing debug output"
simple_title:         "Printing debug output"
programming_language: "Elm"
category:             "Elm"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why?

Printing debug output involves displaying data to the console during the execution of a program. Programmers do this to track how data changes in real time, spot bugs, and understand system behavior.

## How to:

In Elm, use the `Debug.log` function. Assume we have a list of numbers:

```Elm
data = [1, 2, 3, 4, 5]
```

If you want to print this list to the console during your program execution:

```Elm
processed = Debug.log "Data dump" data
```

When you run this, you'll see on your console:

```Elm
Data dump: [1,2,3,4,5]
```

## Deep Dive

Elm's `Debug.log` dates back to the early versions of the language. It's inspired by console.log in JavaScript. While it serves a similar role, Elm's debug tool maintains the language’s philosophy of readability and simplicity.

Elm also offers `Debug.todo` and `Debug.toString`. `Debug.todo` halts execution and displays a message, ideal for marking unimplemented spots. `Debug.toString` converts complex data types to strings for easy viewing.

Note: Debug functions should only be used in a development environment; they are not intended for production use!

## See Also

Check out Elm’s official [Debug module documentation](https://package.elm-lang.org/packages/elm/core/latest/Debug) for more details. Also, [Elm Discourse](https://discourse.elm-lang.org/) is a great place to join discussions and seek help.