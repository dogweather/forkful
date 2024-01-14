---
title:                "Elm recipe: Writing to standard error"
programming_language: "Elm"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Why

Every programmer knows that debugging can be a tedious and time-consuming task. Unfortunately, even with the most thorough testing, it is inevitable to come across unexpected errors in our code. As an Elm programmer, you have probably encountered the dreaded "Red Screen of Death" at some point in your development. That's where writing to standard error comes in - it allows us to get more detailed information about the error, making debugging a lot easier.

## How To

Writing to standard error in Elm is a simple process that can save us a lot of time and effort. We can achieve this by using the `Debug.log` function, which takes in a string and any type of value we want to log. Let's take a look at an example:

```Elm
square : Int -> Int
square x = 
  x * x

main : 
  Html msg
main = 
  square "10"
```

If we try to run this code, we will receive a type mismatch error. However, the error message is not very informative - it just says "Ran into a problem with the type annotation for `main`". By using `Debug.log`, we can get more information about the error:

```Elm
square : Int -> Int
square x =
  Debug.log "Input value:" x
  x * x

main : Html msg
main =
  square "10"
```

Now, when we run the code, we will get the following output in our browser console:

```
Input value: "10"
-- TYPE MISMATCH ----------------------------------------------------- src/Main.elm

The argument to function `square` is causing a mismatch.

10 is an Int

But the type annotation on `square` says it expects the argument to be:

Int
```

This tells us exactly what the input value was and what type of value was expected, making it easier to pinpoint the issue.

## Deep Dive

To better understand how writing to standard error works, it's important to know a bit about console logging in JavaScript. When we use `Debug.log`, the Elm compiler will convert it to a `console.log` call in JavaScript. This means we can also use all the features available in the console, such as using different log levels or styling our output.

Additionally, we can also use `Debug.log2`, `Debug.log3`, and so on, to log multiple values in one call. This can come in handy when we want to log a function with multiple parameters, for example.

## See Also

- [Elm's Debug package documentation](https://package.elm-lang.org/packages/elm/core/latest/Debug)
- [Introduction to Console Logging in JavaScript](https://www.digitalocean.com/community/tutorials/how-to-use-console-log-in-javascript)
- [Debugging Tips and Tricks in Elm](https://dev.to/mostran/debugging-tips-and-tricks-in-elm-1ab)

By using writing to standard error in our Elm code, we can save ourselves a lot of time and frustration when it comes to debugging. So next time you encounter an error, don't forget to use `Debug.log` to get more detailed information. Happy coding!