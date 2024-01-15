---
title:                "Writing to standard error"
html_title:           "Clojure recipe: Writing to standard error"
simple_title:         "Writing to standard error"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Why

Have you ever encountered an error while coding and wished you could have more information about what went wrong? Writing to standard error in Clojure allows you to do just that, providing valuable insights into your code's execution and errors.

## How To

```Clojure
(defn divide [x y]
  (if (zero? y)
    (println "ERROR: Cannot divide by zero!")
    (println (/ x y))))

(defn -main []
  (try
    (divide 10 2)
    (divide 5 0)
    (divide 8 4))
  (catch ArithmeticException e
    (e.printStackTrace)))
```

Output:

```Clojure
5
ERROR: Cannot divide by zero!
2
#<ArithmeticException java.lang.ArithmeticException: Divide by zero>
```

The `println` function in Clojure prints its arguments to standard output by default. However, by using `e.printStackTrace`, we can print the error to standard error instead. This allows us to distinguish between regular program output and error messages.

## Deep Dive

Writing to standard error in Clojure can be useful when debugging and troubleshooting your code. By writing error messages to standard error, you can separate them from regular program output and easily identify where errors occurred in your code. Additionally, standard error can be redirected to a file for later analysis.

It's important to note that on some platforms, standard output and standard error may be displayed in different colors in the terminal, making it easier to differentiate between the two. This can be especially helpful when dealing with large amounts of log data.

See Also

- [Clojure official website](https://clojure.org/)
- [Clojure for the Brave and True by Daniel Higginbotham](https://www.braveclojure.com/clojure-for-the-brave-and-true/) 
- [ClojureDocs](https://clojuredocs.org/)