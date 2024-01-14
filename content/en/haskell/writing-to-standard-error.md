---
title:                "Haskell recipe: Writing to standard error"
simple_title:         "Writing to standard error"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Why

In the world of programming, debugging is an essential task that can often take up a significant amount of time. When writing code in Haskell, it's crucial to have effective error handling mechanisms in place to make debugging easier. One such mechanism is writing to standard error, which allows for more detailed error messages and easier debugging.

## How To

To write to standard error in Haskell, we can use the `hPutStrLn` function from the `System.IO` module. This function takes in a `Handle` and a `String` and writes the string to the given handle. In the case of standard error, the handle can be accessed using `stderr`.

Let's take a look at an example:

``` Haskell
import System.IO (hPutStrLn, stderr)

main = do
    -- Some code that may potentially throw an error
    result <- someFunction
    case result of
        Left err -> hPutStrLn stderr $ "An error occurred: " ++ err
        Right val -> putStrLn "Everything is fine."
```

In this example, we have a `main` function where we are calling a function `someFunction` that can potentially throw an error. We use the `case` statement to handle the `Left` and `Right` cases, and in the `Left` case, we use `hPutStrLn` to write the error message to standard error. This way, we can get a detailed error message in case something goes wrong.

The output of this program will depend on the result of `someFunction`, but in the case of an error, it will be something like this:

```
An error occurred: Division by zero
```

## Deep Dive

When writing to standard error, it's essential to understand how it differs from writing to standard output. Standard output is usually used for printing regular program output, while standard error is reserved for error messages. By writing to standard error, we can ensure that our error messages are not mixed up with regular program output, making it easier to spot and debug errors.

Additionally, when writing to standard error, it's important to handle any exceptions that may occur to prevent the program from crashing. We can do this by using the `catch` function from the `Control.Exception` module. It takes in an `IO` action and a function that handles any exceptions that may occur.

## See Also

- [Haskell IO - Handle and HandleMode](https://www.tutorialspoint.com/haskell/haskell_io_handle_handlemode.htm)
- [Learn You a Haskell - Input and Output](http://learnyouahaskell.com/input-and-output)
- [Control.Exception - Hackage](https://hackage.haskell.org/package/base-4.14.0.0/docs/Control-Exception.html)