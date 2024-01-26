---
title:                "Refactoring"
date:                  2024-01-25T02:12:13.409305-07:00
model:                 gpt-4-1106-preview
simple_title:         "Refactoring"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/refactoring.md"
---

{{< edit_this_page >}}

## What & Why?

Refactoring is the process of restructuring existing computer code without changing its external behavior, aimed at improving nonfunctional attributes. Programmers refactor to make their code cleaner, more efficient, and easier to maintain, effectively enhancing the readability and reducing the complexity of their software.

## How to:

Refactoring in Clojure—thanks to its clean syntax and functional paradigm—can be incredibly straightforward. Let's tackle a common scenario: iterating over collections. You might start with a `for` loop, like so:

```clojure
(defn calculate-sum [numbers]
  (reduce + 0 numbers))

(defn old-way []
  (let [nums (range 1 11)]
    (calculate-sum nums)))
```

Calling `(old-way)` will give us 55, the sum from 1 to 10. But, hey, we can refactor this to be more Clojure-esque:

```clojure
(defn new-way []
  (->> (range 1 11)
       (reduce +)))
```

This refactored `(new-way)` function uses threading macros to pass the range directly into `reduce`, trimming the excess fat.

## Deep Dive

The art of refactoring has its roots in the early days of software development but really gained traction with Martin Fowler's seminal book "Refactoring: Improving the Design of Existing Code" published in 1999. In Clojure, refactoring often leans on functional programming principles, favoring pure functions and immutable data structures.

Alternatives to manual refactoring in Clojure could include using tools like Cursive, a popular IntelliJ IDEA plugin, that offers automated refactors specific to Clojure. There's also clj-refactor, an Emacs package for Clojure, providing a suite of refactoring functions.

A challenge peculiar to refactoring in Clojure is dealing with state and side-effects in a principally immutable and side-effect free paradigm. Careful use of atoms, refs, agents, and transients are pivotal in maintaining both performance and correctness during refactorings.

## See Also

- Martin Fowler's "Refactoring: Improving the Design of Existing Code" for the foundational concepts.
- [Clojure Docs](https://clojuredocs.org/) for specific examples of idiomatic Clojure code.
- [clj-refactor](https://github.com/clojure-emacs/clj-refactor.el) for refactoring automation in Emacs.
- [Cursive](https://cursive-ide.com/) for IntelliJ users seeking automated refactoring assistance.
- [Refactoring with Rich Hickey](https://www.infoq.com/presentations/Simple-Made-Easy/) - A talk by Clojure's creator that, while not about refactoring per se, provides insight into the Clojure philosophy which can guide effective refactoring decisions.
