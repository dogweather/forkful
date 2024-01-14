---
title:    "Clojure recipe: Writing to standard error"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/clojure/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Why
Writing to standard error is a useful technique in Clojure for debugging and error handling. It allows developers to print out important messages or errors to the console, providing valuable information during the coding process.

## How To

To write to standard error in Clojure, we can use the `println` function with the `System/err` argument. This will print our message to the standard error stream instead of the standard output stream.

```
Clojure
(println "This is an error message" System/err)
```

The output of the above code will be:

```
This is an error message
```

By using `System/err` as an argument, we can easily distinguish our error messages from regular output, making it easier to identify and fix issues in our code.

## Deep Dive
In Clojure, standard error is represented by the `java.io.PrintWriter` class. This class allows us to write to the error stream using methods such as `println` or `printf`.

We can also use `System/setErr` to change the default error stream to our own PrintWriter object. This gives us more control over how our error messages are displayed, allowing us to add formatting or additional information.

It's worth noting that in Clojure, exceptions are also printed to the standard error stream by default. This provides important information about the location and cause of the error, making it easier to debug our code.

## See Also
- Official Clojure documentation on PrintWriter - https://clojuredocs.org/clojure/java.io/PrintWriter
- Practical examples of writing to standard error in Clojure - https://www.baeldung.com/java-write-to-standard-error
- An in-depth explanation of standard output and standard error streams in Clojure - https://clojuredocs.org/clojure.core/println#error_handling_and_debugging