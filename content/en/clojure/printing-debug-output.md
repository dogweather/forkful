---
title:    "Clojure recipe: Printing debug output"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Why
As developers, we often encounter bugs and errors in our code. One of the most common ways to identify and fix these issues is by adding debug output to our code. This allows us to see the values of variables and trace through the execution of our code, making it easier to pinpoint the source of the problem.

## How To
To add debug output in Clojure, we can use the `println` function. Let's take a look at an example:

```Clojure
(defn add [x y]
  (println "Adding" x "and" y)
  (+ x y))
```

First, we define a function called `add` which takes in two parameters `x` and `y`. Next, we use `println` to print a message along with the values of our parameters. Finally, we use the `+` function to add `x` and `y`, and return the result.

When we call this function with the values `2` and `3`, we will see the following output in our console:

```
Adding 2 and 3
```

This output allows us to verify that our function is receiving the correct values and performing the intended operation.

## Deep Dive
In addition to the `println` function, Clojure also has the `prn` function which prints the representation of the value. This can be useful when working with data structures such as maps and lists, as it will print a more readable and structured output.

There is also the `println-str` function which converts the arguments into strings and returns them as a single string. This can be helpful if you want to store the output in a variable or use it as part of an error message.

Another tip for debugging in Clojure is to use indentation and line breaks in your debug output message. This can help with visualizing nested data structures and make it easier to read and understand the output.

## See Also
Here are some helpful resources for debugging in Clojure:

- [Clojure Debugging with REPL](https://clojurescriptmadeeasy.com/blog/clojure-debugging-with-repl/)
- [Debugging in Clojure with print and println](https://www.baeldung.com/clojure-debugging-print-println)
- [Debugging Functions in Clojure](https://www.braveclojure.com/debugging-functions/)