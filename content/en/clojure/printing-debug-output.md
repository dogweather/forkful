---
title:    "Clojure recipe: Printing debug output"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/clojure/printing-debug-output.md"
---

{{< edit_this_page >}}

## Why
Debugging is an essential part of the software development process. It allows developers to track and fix errors in their code. One of the most common techniques used in debugging is printing debug output, which refers to displaying information about the program's execution at various points during runtime. In this blog post, we will explore how to use the `print` function in Clojure to print debug output and why it is a valuable tool for developers.

## How To
Printing debug output in Clojure is simple and straightforward. First, we need to locate the code block where we want to insert our debug statements. Then, we can use the `print` function and pass in the value or expression we want to print as an argument. Let's take a look at an example:

```Clojure
(defn add [a b] (print "Adding two numbers") (+ a b))
```

In the above code, we have a function `add` that takes in two parameters and prints a debug statement before adding them together. Now, let's call this function with two numbers and see the output:

```Clojure
(add 2 3)
```

The output will be:

```
"Adding two numbers"
5
```

As we can see, the `print` statement was executed before the addition operation, and we were able to view the debug information. We can also use the `prn` function, which will print the value and a new line after it.

## Deep Dive
In Clojure, there are a few more options for printing debug output. We can use the `println` function, which prints the value followed by a new line. We can also use the `pr` function, which prints the value without any special formatting. Additionally, we can use the `format` function to control the output format of our debug statements.

It is also worth noting that we can use the `print` function not just for strings but also for any data type, including maps, lists, and even functions. This gives us more flexibility in how we view our debug information and can be useful in complex debugging scenarios.

## See Also
- [Clojure docs on print function](https://clojuredocs.org/clojure.core/print)
- [Debugging in Clojure by Thoughtbot](https://thoughtbot.com/blog/debugging-in-clojure)
- [Mastering Clojure's `print` function](https://www.braveclojure.com/basic-debugging/)

And that's it for printing debug output in Clojure! Remember, using this technique can save you time and make the debugging process smoother, so don't hesitate to use it in your next project. Happy coding!