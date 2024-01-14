---
title:    "Elm recipe: Printing debug output"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/elm/printing-debug-output.md"
---

{{< edit_this_page >}}

## Why

Debugging is a crucial part of any programming task, and Elm is no exception. Printing debug output is an essential tool that can help you understand the inner workings of your program and identify any logical errors or bugs that may be causing unexpected behavior. In this blog post, we will dive into the world of printing debug output in Elm and learn how it can improve your development process.

## How To

To print debug output in Elm, we can use the `Debug.log` function. This function takes in two arguments: a label and any value we want to print. The label is simply a string that helps us identify the specific output we are printing. Let's take a look at an example:

```Elm
import Debug

x = 5
Debug.log "x value" x

-- output:
-- x value: 5
```

In this example, we use the label "x value" and the `x` variable as the value we want to print. As you can see, the output will display the label and the value separated by a colon. This can be useful when trying to track the value of a variable or a particular function's result.

We can also use `Debug.log` in more complex scenarios, such as printing the values of multiple variables or combining strings and values. Let's see an example:

```Elm
import Debug

firstName = "John"
lastName = "Smith"
Debug.log "Full name" (firstName ++ " " ++ lastName)

-- output:
-- Full name: John Smith
```

In this example, we are combining the first and last name variables and printing them with a label. This can be helpful when trying to keep track of different variables' values and their relationships within the program.

## Deep Dive

Now that we have seen how to use `Debug.log`, let's take a deeper look at its inner workings. Whenever we use `Debug.log`, we are essentially creating a `Task` that logs the given value and then continues with the given task. This means that these log tasks will not affect the program's execution or perform any side effects.

However, it is essential to keep in mind that `Debug.log` should only be used for logging purposes during development. It is not recommended to have any `Debug.log` statements in your production code as it can significantly impact performance.

## See Also

To learn more about debugging in Elm, check out the official Elm documentation and the `Debug` library documentation. You can also find helpful tips and techniques in various Elm programming blogs and forums. Happy debugging!