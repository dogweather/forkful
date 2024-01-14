---
title:    "Elm recipe: Writing to standard error"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/elm/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# Why Write to Standard Error?

Writing to standard error (often abbreviated as "stderr") can be a useful tool for debugging and error handling in Elm programming. By printing out specific information to the error stream, developers can gain insight into the code and diagnose issues more easily.

# How To Write to Standard Error in Elm

Writing to standard error in Elm is a straightforward process. The `Debug.log` function allows developers to print a message to stderr, along with any values they want to include for debugging purposes. Here's an example:

```elm
-- Prints "x is 10" to stderr
Debug.log "Debugging" ("x is " ++ toString 10)
```

The above code would produce the following output in the console:

```
Debugging: x is 10
```

# Deep Dive into Writing to Standard Error

The `Debug.log` function takes two arguments: a message string and a value to include in the output. It then returns the value, making it easy to insert into existing code without causing side effects. This can be especially helpful when trying to track the value of a variable over multiple function calls.

Another useful function for writing to standard error in Elm is `Debug.todo`, which is used to signify that a certain part of the code is unfinished and still needs to be implemented. This can be a helpful reminder when working on a large project and allows for easy identification of incomplete tasks.

# See Also

For more information on writing to standard error in Elm, check out these resources:

- [Elm's Official Debug Module Documentation](https://package.elm-lang.org/packages/elm/core/latest/Debug)
- [A Guide to Debugging in Elm](https://dennisreimann.de/articles/elm-debugging.html)
- [Using Debug.log and Debug.todo in Elm](https://medium.com/@sav_ya/using-debuglog-and-debugtodo-in-elm-4829e0be3d96)