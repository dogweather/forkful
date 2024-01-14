---
title:                "Clojure recipe: Starting a new project"
simple_title:         "Starting a new project"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/starting-a-new-project.md"
---

{{< edit_this_page >}}

"
## Why

Starting a new project in Clojure can be an exciting and rewarding experience. Clojure is a dynamic, functional programming language that is gaining popularity in the industry due to its simplicity, conciseness, and powerful features. It allows developers to write robust and scalable code that is easy to maintain. If you're someone who enjoys writing elegant and efficient code, then starting a new project in Clojure might be just the thing for you.

## How To

To get started with Clojure, you will need to have the Java Runtime Environment (JRE) installed on your system. Once that is done, you can follow these simple steps to set up a basic Clojure project:

```Clojure
(defproject my-project "0.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.10.1"]]
  :main ^:skip-aot my-project.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
```

This is a basic project definition file (project.clj) for a Clojure project. It specifies the project name, version, dependencies, and other settings. In this example, we have defined our project to depend on the latest version of Clojure (1.10.1). We have also specified that the main namespace for our project is my-project.core and that we want to skip Ahead-of-Time (AOT) compilation for our main namespace. Finally, we have specified where we want our compiled code to be located.

Next, we can create a new Clojure file (e.g. core.clj) in the src/my_project directory and add some code to it:

```Clojure
(ns my-project.core)

(defn greet [name]
  (str "Hello, " name))
```

This code creates a simple function called greet that takes in a name parameter and returns a string greeting the person by name. We can see this in action by opening up a REPL (Read-Evaluate-Print-Loop) and running the following commands:

```Clojure
(use 'my-project.core)

(greet "John")

;; Output: "Hello, John"
```

The use function is used to load a namespace and its associated code into the current REPL session. We can then call our greet function and see the output.

## Deep Dive

Starting a new project in Clojure also gives you the opportunity to learn and explore its powerful features. Clojure is built on top of the JVM, which means it has access to all the Java libraries and can also integrate with other JVM languages such as Java and Scala. This allows developers to leverage the vast ecosystem of Java libraries and tools while still being able to write concise and functional code.

Clojure also has a strong emphasis on immutability and persistent data structures. This means that data structures are not modified in-place, but rather new versions are created. This makes it easier to reason about and debug code, as well as providing better concurrency support.

Another important feature of Clojure is its emphasis on functional programming. Clojure treats functions as first-class citizens, meaning that they can be passed as arguments, returned as values, and stored in data structures. This allows for a more declarative programming style and makes code more modular and reusable.

## See Also

- Official Clojure website: https://clojure.org
- Clojure documentation: https://clojure.org/documentation
- Clojure Style Guide: https://github.com/bbatsov/clojure-style-guide