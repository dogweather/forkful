---
title:                "Clojure recipe: Writing to standard error"
programming_language: "Clojure"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Why

In Clojure, writing to standard error can be a useful tool for developers. It allows us to output information or error messages directly to the console, helping us to debug our code and identify issues quickly.

## How To

To write to standard error in Clojure, we can use the built-in function `println` with the `System/err` parameter. Here's an example:

```Clojure
(println "This is an error message" System/err)
```

This code will output the string "This is an error message" to the standard error stream. The `println` function automatically appends a new line character, so our error message will be on its own line in the console.

To see this in action, let's create a simple function that will divide two numbers and print an error message if the second number is zero:

```Clojure
(defn divide [numerator denominator]
  (if (= denominator 0)
    (println "Error: Cannot divide by zero" System/err)
    (/ numerator denominator)))
```

Now, when we try to use this function with a zero denominator, the error message will be written to the console:

```Clojure
(divide 10 0)
```

Output:
```Clojure
Error: Cannot divide by zero
```

## Deep Dive

Writing to standard error in Clojure is actually a wrapper around Java's `System.err` output stream. This means that we have access to all the methods defined in the Java API for writing to the standard error stream.

For example, we can use the `print` and `println` methods to write strings or objects to standard error:

```Clojure
(.println System/err "This will be written to standard error")
(.print System/err 5)
```

Output:
```Clojure
This will be written to standard error
5
```

We can also use the `flush` method to ensure that all buffered data is written to the standard error stream:

```Clojure
(.flush System/err)
```

Additionally, we can use the `format` method to write formatted strings to standard error, just like we would with `println`:

```Clojure
(.format System/err "The answer is %d" 42)
```

Output:
```Clojure
The answer is 42
```

## See Also

- [Clojure Documentation on Writing to Standard Error](https://clojure.org/guides/logging#error_logging)
- [Java API for System.err](https://docs.oracle.com/javase/7/docs/api/java/lang/System.html#err)
- [Difference between standard output and standard error](https://www.geeksforgeeks.org/difference-between-standard-output-stream-and-standard-error-stream/)