---
title:                "Printing debug output"
html_title:           "Arduino recipe: Printing debug output"
simple_title:         "Printing debug output"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why?

Printing debug output refers to generating messages to track code execution. Programmers use it to isolate and identify issues, trace code execution, and verify correctness. 

## How to:

In Clojure, you have several options to print debug output. The most common one is using `println`. Let's demonstrate a simple example:

```Clojure
(defn add-numbers [a b]
  (let [sum (+ a b)]
    (println "Debug: " sum)
    sum))
```

When you run `(add-numbers 1 2)`, you will see the following output:

```Clojure
Debug: 3
```

## Deep Dive

The concept of printing debug output has been around since the early days of programming. It's usually valuable during development but can clutter the output in production.

While `println` is an easy way to print debug output, Clojure has alternatives too. One of the popular libraries used for this is `tools.logging`.

Here is how you can do logging using `tools.logging`:

```Clojure
(ns my-ns
  (:require [clojure.tools.logging :as log]))

(defn add-numbers [a b]
  (let [sum (+ a b)]
    (log/info "Debug: " sum)
    sum))
```

Remember, `println` directly outputs to the console, whereas `tools.logging` gives you more control as you can configure where you want to log (console, file, etc.). 

## See Also

Additional information and related topics can be found at:

1. [Tools.Logging Clojure Documentation](https://clojure.github.io/tools.logging/)
2. [Clojure Debugging](https://clojuredocs.org/clojure.repl/printf)
3. [Clojure - A Beginner’s Guide](https://www.braveclojure.com/getting-started/)