---
title:                "Elm recipe: Printing debug output"
programming_language: "Elm"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/printing-debug-output.md"
---

{{< edit_this_page >}}

## Why

Printing debug output can be a useful tool for developers, allowing them to get a better understanding of their code and troubleshoot any potential issues. It can also be helpful in the learning process, as it allows users to see the values and types of their variables at different points in their program.

## How To

To print debug output in Elm, we can use the `Debug.log` function. This function takes in two parameters: a string label and a value. The label acts as a description for the value being printed, and the value can be any type of data. Let's take a look at an example:

```Elm
import Debug exposing (log)

x = 5
y = "Hello, world!"

Debug.log "Value of x:" x
Debug.log "Value of y:" y
```

The output of this code would be:

```
Value of x: 5
Value of y: "Hello, world!"
```

You can also use the `log` function with multiple values by passing them in as a tuple:

```Elm
z = (10, 20)
Debug.log "Values of z:" z
```

The output of this code would be:

```
Values of z: (10, 20)
```

## Deep Dive

The `Debug.log` function can also take in a second argument, which is a function that takes in the value being printed and returns a `String`. This allows for more advanced debugging, as we can format our output in a specific way. Let's see an example:

```Elm
formatValue value =
    "Value is " ++ (toString value)

x = 15
Debug.log "Formatted value:" x formatValue
```

The output of this code would be:

```
Formatted value: Value is 15
```

We can also use the `List.concatMap` function to print out the values of a list. Let's take a look at an example:

```Elm
list = [1,2,3,4]
Debug.log "List values:" list (List.concatMap toString)
```

The output of this code would be:

```
List values: 1 2 3 4
```

## See Also

- [Debug module in Elm](https://package.elm-lang.org/packages/elm/core/latest/Debug)
- [Using debug.log in Elm](https://guide.elm-lang.org/debugging/debugging.html)
- [Debugging tips and tricks in Elm](https://www.stephenpfrank.com/posts/2015-09-02-elm-debugging-tips-tricks.html)