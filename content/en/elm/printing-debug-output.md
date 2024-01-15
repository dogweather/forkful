---
title:                "Printing debug output"
html_title:           "Elm recipe: Printing debug output"
simple_title:         "Printing debug output"
programming_language: "Elm"
category:             "Elm"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/printing-debug-output.md"
---

{{< edit_this_page >}}

## Why
Debugging is an essential part of any programming task. It helps find and fix errors in the code, making it easier to develop and maintain software. In some cases, it may be necessary to print out specific values or messages to track the flow of the program and identify potential issues. This is where printing debug output comes in handy.

## How To
To print debug output in Elm, we can use the built-in `Debug` module which provides functions for logging values and messages to the console. Let's take a look at some examples:

```
import Debug exposing (log)

-- Printing a value
log "Hello, world!"

-- Printing a message with a value
log "The answer is" 42

-- Printing a list of values
log "Fruits" ["apple", "banana", "orange"]
```

The output for these examples will be displayed in the browser's developer console. If you're using Chrome, you can access it by pressing `Ctrl + Shift + J` on Windows or `Command + Option + J` on Mac.

```
Hello, world!
The answer is 42
Fruits
    ["apple","banana","orange"]
```

Additionally, we can use the `logMany` function to print multiple values or messages in a single line.

```
import Debug exposing (logMany)

logMany [ "Name" "John", "Age" 26 ]
```

This will print out the following in the console:

```
Name | "John" -- Age | 26
```

## Deep Dive
The `Debug` module also provides an `crash` function which is useful for debugging unexpected errors in the code. It will cause the program to crash and display the error in the console.

Another useful function is `todo` which can be used as a placeholder for unfinished code. It will print a warning in the console, reminding us to come back and complete the code later.

We can also further customize the debug output by using `fancyLog` and `toString`. `fancyLog` allows us to add labels and colors to our messages, while `toString` converts complex data structures into a string representation for easier debugging.

## See Also
For more information on debugging in Elm, check out the following resources:

- [Elm Debug module documentation](https://package.elm-lang.org/packages/elm/core/latest/Debug)
- [Elm official guide on debugging](https://guide.elm-lang.org/debugging/)
- [Debugging Elm with VSCode](https://blog.noredink.com/post/121278106448/debugging-elm-in-vs-code)

Keep in mind that excessive use of `Debug.log` can make the code harder to read and maintain. It should only be used for specific purposes and always removed before deploying to production. Happy debugging!