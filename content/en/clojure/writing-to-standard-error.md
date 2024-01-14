---
title:    "Clojure recipe: Writing to standard error"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Why

Have you ever encountered errors or issues while coding in Clojure? Writing to standard error can be a useful tool for debugging and troubleshooting your code. It allows you to print error messages and information to your terminal, making it easier to identify and fix any problems in your program.

## How To

To write to standard error in Clojure, we can use the ```println``` function and specify the output stream as ```System/err```. Let's take a look at an example:

```Clojure
(println "Error: Invalid input" System/err)
```

This code will print the error message "Error: Invalid input" to the standard error stream. Note that we are using the ```System/err``` keyword to specify the output stream as standard error.

We can also use the ```prn``` function to print the error message without a new line character at the end. This can be useful when we want to print multiple messages to the same line. Let's see an example:

```Clojure
(prn "Error:")
(prn "Invalid input" System/err)
```

This will print "Error: Invalid input" on the same line in the standard error stream.

## Deep Dive

In Clojure, writing to standard error is also useful when working with exceptions. We can use the ```ex-info``` function to create and print custom exception messages to standard error. Here's an example:

```Clojure
(defn divide [a b]
  (if (zero? b)
    (throw (ex-info "Cannot divide by zero" {:numerator a, :denominator b}))
    (/ a b)))

(try
  (divide 10 0)
  (catch Exception e
    (prn "Exception:" e System/err)))
```

In this code, we created a custom exception message using ```ex-info``` and printed it to standard error using ```prn```. This allows us to handle exceptions more efficiently and provide useful information for debugging.

## See Also

To learn more about writing to standard error in Clojure, check out the following resources:

- Official Clojure documentation on writing to standard error: https://clojure.org/reference/java_interop#_writing_to_standard_error

- A helpful tutorial on using ```ex-info``` for custom exceptions: https://purelyfunctional.tv/lesson/using-ex-info/

- Clojure Cookbook's section on error handling: https://clojure-cookbook.com/], s-err.html