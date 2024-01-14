---
title:                "Clojure recipe: Printing debug output"
simple_title:         "Printing debug output"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/printing-debug-output.md"
---

{{< edit_this_page >}}

# Why Print Debug Output?

When coding in Clojure, it is often necessary to debug and troubleshoot code to ensure it is functioning correctly. This is where printing debug output comes in. By printing out specific values or variables during runtime, developers can better understand what their code is doing and identify any errors or issues.

# How To Print Debug Output

To print debug output in Clojure, we can use the `println` function. This function takes in one or more arguments and prints them to the console.

```Clojure
; Printing a single value
(println "Hello World!")
; Output: Hello World!

; Printing multiple values
(println "Clojure" "is" "awesome")
; Output: Clojure is awesome
```

We can also use `str` function to convert values to strings before printing them.

```Clojure
; Printing a string with a variable
(def name "John")
(println (str "Hello " name))
; Output: Hello John
```

Sometimes, we may need to print values within a larger expression. In this case, we can use the `prn` function which prints the values without adding a new line.

```Clojure
; Printing values within an expression
(println "The sum of 2 and 3 is" (prn (+ 2 3)))
; Output: The sum of 2 and 3 is 5
```

# Deep Dive Into Printing Debug Output

Printing debug output is a useful technique for troubleshooting and understanding code, but it should not be overused. Excessive use of `println` or `prn` statements can clutter the code and make it difficult to read. It is important to only use them when necessary and remove them once the code is functioning correctly.

Additionally, for more complex debugging, Clojure offers tools like `clojure.repl` and `clojure.tools.trace` which allow developers to step through code and see the values at each step.

# See Also

- [Clojure official documentation on print functions](https://clojure.org/reference/reader#_print_functions)
- [How to Debug Clojure Code](https://www.clojure.org/guides/debugging)
- [Clojure Debugging Tools](https://github.com/mfikes/clojure-debugger)