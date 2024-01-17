---
title:                "Printing debug output"
html_title:           "Clojure recipe: Printing debug output"
simple_title:         "Printing debug output"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why?

Printing debug output is the act of displaying information during program execution to help identify and understand issues that may arise. Programmers use this technique to get a better understanding of the code's behavior and to troubleshoot errors or unexpected results.

## How to:

To print debug output in Clojure, you can use the ```clojure/pprint``` library. This library provides functions that pretty-print data structures, making it easier to read and understand. Here's an example:

```
(clojure/pprint (range 5))
;; Output: (0 1 2 3 4)
```

You can also use the ```println``` function, which prints a line to the standard output. Here's an example:

```
(println "Hello World!")
;; Output: Hello World!
```

## Deep Dive:

Printing debug output has been a common practice in the programming world, and it remains relevant today. In the past, developers would often use debugging tools or print statements to troubleshoot issues. With the rise of unit testing and integrated development environments, the need for printing debug output has decreased. However, it is still a useful technique, especially for understanding the behavior of complex code or for debugging errors that are difficult to recreate.

Apart from the ```clojure/pprint``` library and the ```println``` function, there are other ways to print debug output in Clojure. For example, you can use the ```prn``` function, which prints its arguments without a newline. Additionally, Clojure has a built-in ```print-dup``` function, which prints the serializable representation of data structures. These alternatives provide more flexibility depending on the debugging task at hand.

Implementation-wise, printing debug output in Clojure is relatively straightforward. The ```clojure/pprint``` library uses the ```clojure.pprint``` namespace, which provides functions for pretty-printing. Similarly, the ```println``` function is defined in the ```clojure.core``` namespace. Understanding how these functions work under the hood can give you a better grasp of the debugging process.

## See Also:

To learn more about printing debug output in Clojure, check out the official documentation for the ```clojure/pprint``` library and the ```println``` and ```prn``` functions. You can also explore other debugging techniques and tools, such as unit testing and integrated development environments, to find the best approach for your development workflow. Happy debugging!