---
title:                "Writing to standard error"
html_title:           "Elm recipe: Writing to standard error"
simple_title:         "Writing to standard error"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Why

Are you tired of your Elm code being perfect and error-free all the time? Do you miss the thrill of debugging and troubleshooting? Then writing to standard error is the solution for you! With standard error, you can intentionally create errors and see how your code handles them. It's a great way to test and improve your code.

## How To

Writing to standard error in Elm is simple. Just follow these steps:

1. Import the `Debug` module by adding `import Debug` at the top of your code.
2. Use the function `Debug.crash` to create an error. You can pass in a custom error message as a parameter.
3. If you want to include a custom value in the error message, you can use string interpolation with `Debug.toString` to convert the value to a string.
4. To view the error message and value, run your Elm code in a browser and open the console.

Here is an example of code with writing to standard error:

```Elm
import Debug

myAge : Int
myAge = 25

Debug.crash ("I am " ++ (Debug.toString myAge) ++ " years old.")
```

This will create an error in the console that says "I am 25 years old."

## Deep Dive

When you use `Debug.crash`, a `Debug.crash` function is called, which in turn calls the Elm function `Native.Debug.crash`. This function creates a JavaScript error with the specified message and value. This error is then caught and displayed in the console.

One thing to note is that using `Debug.crash` is not recommended for production code. It should only be used for testing and debugging purposes.

## See Also

- [Elm's official documentation on Debugging](https://elm-lang.org/docs/debugging)
- [The Debug module in Elm's standard library](https://package.elm-lang.org/packages/elm-lang/core/latest/Debug)