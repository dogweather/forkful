---
title:                "Clojure recipe: Writing to standard error"
simple_title:         "Writing to standard error"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Why

At some point in your coding journey, you may have encountered the term "standard error" or "stderr". But what exactly is it and why would you want to write to it in your Clojure programs? Simply put, standard error is a stream of data that is used to output error messages from a program. Writing to standard error allows you to provide valuable information to help debug and troubleshoot your code.

## How To

Writing to standard error in Clojure is a simple task. Let's take a look at the following code snippet:

```Clojure
(defn divide [a b]
  (if (zero? b)
    (println "Error: Cannot divide by zero")
    (println "The result is" (/ a b)))
```

In this example, we have a function called `divide` that takes two parameters, *a* and *b*. We use the `if` statement to check if *b* is equal to zero. If it is, we print an error message to standard error using `println`. Otherwise, we print the result of dividing *a* by *b* to standard output. Let's take a look at the output of this function:

```Clojure
(divide 10 2)
The result is 5
(divide 10 0)
Error: Cannot divide by zero
```

As you can see, the error message is printed to standard error while the result is printed to standard output. This separation allows for easier debugging and understanding of the flow of your code.

## Deep Dive

In Clojure, there are two main ways to write to standard error: `println` and `eprintln`. The `println` function will print to standard output while the `eprintln` function will print to standard error. Additionally, you can use the `with-out-str` and `with-err-str` functions to capture and manipulate the output of `println` and `eprintln` respectively.

It's important to note that writing to standard error should be used for important, critical, or error-related messages, while standard output should be used for general messages and data.

## See Also

- [Official Clojure Documentation on println and eprintln](https://clojure.github.io/clojure/clojure.core-api.html#println)
- [Clojure for the Brave and True: Writing to Standard Output and Error](https://www.braveclojure.com/inside-clojure-standard-io/)

Now that you have a better understanding of writing to standard error in Clojure, try implementing it in your own programs and see how it can improve your debugging process. Happy coding!