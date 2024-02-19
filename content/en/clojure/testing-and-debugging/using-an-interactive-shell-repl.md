---
aliases:
- /en/clojure/using-an-interactive-shell-repl/
date: 2024-01-25 03:39:48.374381-07:00
description: "REPL, or Read-Eval-Print Loop, is a programming environment for dynamically\
  \ testing Clojure code piece by piece. Coders use it for immediate feedback,\u2026"
lastmod: 2024-02-18 23:09:10.725439
model: gpt-4-1106-preview
summary: "REPL, or Read-Eval-Print Loop, is a programming environment for dynamically\
  \ testing Clojure code piece by piece. Coders use it for immediate feedback,\u2026"
title: Using an interactive shell (REPL)
---

{{< edit_this_page >}}

## What & Why?
REPL, or Read-Eval-Print Loop, is a programming environment for dynamically testing Clojure code piece by piece. Coders use it for immediate feedback, iterative development, and quick experimentation without the overhead of compiling or setting up a complete project environment.

## How to:
Start with launching REPL:

```Clojure
user=> (println "Hello, REPL!")
Hello, REPL!
nil
```

Define a function and try it out:
```Clojure
user=> (defn greet [name] (str "Hello, " name "!"))
#'user/greet
user=> (greet "Clojure Programmer")
"Hello, Clojure Programmer!"
```

Experiment with data structures:
```Clojure
user=> (def my-map {:a 1 :b 2})
#'user/my-map
user=> (assoc my-map :c 3)
{:a 1, :b 2, :c 3}
```

## Deep Dive
The REPL is key to the Lisp family's interactive development philosophy, and Clojure, a modern Lisp dialect, makes great use of this tool. It dates back to the first Lisp REPL in the late 1950s. Alternatives in other languages include Python's interpreter and Node.js's console, but Clojure's REPL has first-class status and is integral to the workflow.

A Clojure REPL session can be integrated into various environments like command-line, IDEs (such as IntelliJ with Cursive, or Emacs with CIDER), or browser-based tools like Nightcode. In a deeper sense, the REPL empowers the developer to manipulate the language's constructs at run-time and carry states across various transformations, often leading to exploratory programming and more robust code.

The REPL's functionality shines with tools like `lein repl` or `clj`, which allow for dependency management, various plugins, and project-specific customizations, leading to a more productive and flexible development process.

## See Also
- The official Clojure website guide on the REPL: https://clojure.org/guides/repl/introduction
- Rich Hickey's talk about REPL-driven development: https://www.youtube.com/watch?v=Qx0-pViyIDU
- Practical Clojure: using the REPL for iterative development: http://practicalclj.blogspot.com/2009/10/using-clojure-repl.html
