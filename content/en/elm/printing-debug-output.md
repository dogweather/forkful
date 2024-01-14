---
title:    "Elm recipe: Printing debug output"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Why

Debugging is an essential part of any programming language, and Elm is no exception. Often, we might find ourselves facing unexpected results or errors in our code, and this is where printing debug output comes in handy. It allows us to gain insights into what our code is doing and helps us identify any issues or bugs that may be causing problems.

## How To

To print debug output in Elm, we can make use of the `Debug.log` function. It takes in two arguments: a string label and a value to be printed. Let's look at an example of how we can use `Debug.log` in our code:

```
Elm.Code
Debug.log "Number" 5
```

In the code block above, we are passing in the label "Number" and the value 5 to be printed. If we run this code, we will see the following output in our console:

```
Number: 5
```

This allows us to see the value of our variable at a specific point in our code and track its changes as our code executes.

We can also use `Debug.log` within functions to print the values of different variables or inputs. Let's take a look at an example of this:

```
Elm.Code
tupleSum x y =
  let
    sum = x + y
  in
    Debug.log "Sum" sum

tupleSum 2 3
```

In this code, we have defined a function called `tupleSum` that takes in two numbers and returns their sum. We are using `Debug.log` within the function to print the value of the sum variable. When we call the function with the inputs 2 and 3, we will see the following output:

```
Sum: 5
```

This shows us that our function is working correctly and that the value of the sum variable is 5.

## Deep Dive

Apart from using `Debug.log` for basic debugging purposes, we can also use it to gain a deeper understanding of our code. For example, we can print out the values of different variables within a loop to see how they are changing with each iteration. This can be particularly helpful when dealing with complex algorithms that involve a lot of calculations.

We can also use `Debug.log` to print out the values of nested data structures, such as lists and dictionaries, to see their structure and contents. This can be useful when troubleshooting issues with data manipulation.

Another way to utilize `Debug.log` is by printing out values from different parts of our code to see how they relate to each other. This can help us identify any potential logic errors and fix them before they cause problems in our application.

## See Also

Here are some useful resources for further reading on printing debug output in Elm:

- [Elm Debugging Basics](https://guide.elm-lang.org/debugging/)

- [Debug Module Documentation](https://package.elm-lang.org/packages/elm/debug/latest/Debug)

- [Debugging In Elm: Tools And Techniques](https://dennisreimann.de/articles/debugging-in-elm.html)