---
title:    "Elm recipe: Writing to standard error"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Why

Writing to standard error, also known as `stderr`, is an important feature in Elm programming. It allows us to log errors and warnings in a separate stream from regular output, making it easier to debug our code and improve its reliability.

## How To

To write to `stderr` in Elm, we can use the `Debug.log` function. Here's an example:

```Elm
import Debug exposing (log)

-- A function that divides two numbers
divide : Float -> Float -> Float
divide x y =
  log "Dividing" (x / y)
```

In this example, we are using `Debug.log` to log the result of our division operation to `stderr`. This function takes in a label (in this case, "Dividing") and the value we want to log (in this case, `x / y`). When we run our code, the output in the console will be:

```
Dividing: 2.5
```

This allows us to easily track what values are being passed into our functions and debug any potential issues.

## Deep Dive

When using `Debug.log`, it's important to note that the label argument is evaluated as a function. This means if we want to log a variable, we need to use the `_` placeholder in the label. For example:

```Elm
import Debug exposing (log)

-- A function that divides two numbers
divide : Float -> Float -> Float
divide x y =
  log (toString x ++ " / " ++ toString y) (x / y)
```

In this example, we are using the `_` placeholder in the label to log the values of `x` and `y` without calling them as functions. This will result in an output of:

```
5.0 / 2.0: 2.5
```

Additionally, we can also use `Debug.log` as a "debugging print statement" to track the flow of our code and check if certain conditions are being met. This can be especially useful in more complex applications where it's not always easy to follow the data flow.

## See Also

For more information on writing to `stderr` in Elm, check out these resources:

- [Elm Syntax: Debug.log](https://elm-lang.org/docs/syntax#debug-log)
- [Elm Radio: Debug.module](https://elm-radio.com/episode/debug-module)
- [Elm in Action: Debugging with Standard Error](https://livebook.manning.com/book/elm-in-action/chapter-11/38)