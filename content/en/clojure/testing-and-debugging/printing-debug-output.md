---
date: 2024-01-20 17:52:15.932187-07:00
description: 'How to: In Clojure, you often print debug output using `println`, `printf`,
  `pr`, or `prn`. Here''s how you sprinkle in some debug prints.'
lastmod: '2024-03-13T22:44:59.748025-06:00'
model: gpt-4-1106-preview
summary: In Clojure, you often print debug output using `println`, `printf`, `pr`,
  or `prn`.
title: Printing debug output
weight: 33
---

## How to:
In Clojure, you often print debug output using `println`, `printf`, `pr`, or `prn`. Here's how you sprinkle in some debug prints:

```Clojure
(defn add-and-print [a b]
  (println "Adding:" a "and" b) ; Prints the operation
  (let [result (+ a b)]
    (println "Result:" result)  ; Prints the result
    result))                    ; Returns the result

(add-and-print 3 4)
```
Sample Output:
```
Adding: 3 and 4
Result: 7
```

Or, to debug values in the middle of a threading macro:

```Clojure
(require '[clojure.pprint :refer [pprint]])

(-> 3
    (+ 5)
    (pprint)             ; Prints intermediate result
    (* 2))
```
Sample Output:
```
8
```

## Deep Dive:
Print debugging has a long history, probably as old as programming itself. It's straightforward: you insert print statements where you suspect problems might lie, run the code, and look at the output.

Clojure's functions for debug printing are quite similar to those in other Lisp languages, but with the usual functional flavor. `println` and `prn` differ in that the latter writes data in a way that can be read by the Clojure reader. `pprint` (pretty print) from `clojure.pprint` can be used when you want a nicer format.

A Clojure-specific tool for debugging is `tap>`. Introduced in Clojure 1.10, it allows for non-blocking 'taps' into running code without having to litter your code with print statements.

For larger or more complex projects, consider a logging library like `clojure.tools.logging` or `timbre`.

## See Also:
- [`clojure.tools.logging`](https://github.com/clojure/tools.logging) GitHub repository
- [Timbre logging library](https://github.com/ptaoussanis/timbre) GitHub repository
- [`clojure.pprint`](https://clojuredocs.org/clojure.pprint/pprint) documentation on ClojureDocs
