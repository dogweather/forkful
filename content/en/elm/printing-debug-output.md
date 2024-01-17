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

## What & Why?

Printing debug output is a way for programmers to see the values of variables and data structures while their code is running. This is often used for troubleshooting and understanding the flow of the code.

## How to:

To print debug output in Elm, you can use the built-in `Debug.log` function. It takes two arguments - a message to display and the value to be printed. For example:

```elm
Debug.log "My variable is" myVariable
```

This will print "My variable is" followed by the value of `myVariable` in the console when the code is executed.

You can also use `Debug.toString` to convert any value into a string and then print it using `Debug.log`. For example:

```elm
Debug.log "My list is" (Debug.toString myList)
```

This will print "My list is" followed by the string representation of `myList` in the console.

## Deep Dive

Printing debug output has been a common practice in programming for a long time. It allows developers to understand what is happening in their code and quickly identify and fix any issues.

There are alternative methods to printing debug output, such as using a debugger or adding `Debug.todo` statements in code to mark areas that need attention. However, printing debug output is a simple and straightforward way to get insights into the code.

Internally, the `Debug.log` function uses the `Debug.watch` command from the Elm runtime to print the message and value to the console. This command can also be used directly in code for more advanced debugging purposes.

## See Also

- [Elm Debug module](https://package.elm-lang.org/packages/elm-lang/core/latest/Debug)
- [Debugging in Elm](https://guide.elm-lang.org/debugging/)