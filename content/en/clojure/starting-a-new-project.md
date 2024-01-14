---
title:                "Clojure recipe: Starting a new project"
programming_language: "Clojure"
category:             "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Why

If you're a developer looking to expand your programming skills and try something new, then starting a project in Clojure might be the perfect challenge for you. Clojure is a functional programming language that offers a unique and powerful approach to developing applications. It's also gaining popularity in the tech industry, making it a valuable skill to add to your repertoire.

## How To

To get started with a Clojure project, you will need to have the Clojure toolchain installed on your machine. This includes the Clojure command line tools and a Java runtime environment. Once you have everything set up, you can start a new project by following these steps:

1. Open up your terminal and navigate to the directory where you want your project to be located.
2. Run the command `clj -X:new :name <project-name>`. This will create a new project using a template provided by the Clojure community.
3. Next, navigate into the newly created project directory using `cd <project-name>`.
4. You can now start coding your project using your preferred code editor. It's recommended to use an editor that has support for Clojure, such as Emacs or IntelliJ.
5. To run your project, use the command `clj -X:run`. This will start your application and output any results.

Here's a simple "Hello World" example in Clojure:

```Clojure
(ns my-project.core)

(defn -main []
  (println "Hello World!"))
```

Running this code would output `Hello World!` in the terminal. 

## Deep Dive

Now that you have your project set up and some basic knowledge of how to run it, it's time to dive deeper into Clojure. One of the key features of Clojure is its focus on immutability and functional programming. This means that data structures in Clojure are immutable by default and functions are treated as first-class objects. This allows for writing concise and elegant code that is easy to reason about and test.

Another important aspect of Clojure is its use of the Lisp syntax. This may take some getting used to for developers coming from more traditional languages, but once mastered, it offers powerful and flexible ways of manipulating code. As an example, Clojure supports macros, which are code transformations that can be used to create new syntax or optimize code at compile-time.

See Also

- [Clojure.org](https://clojure.org/) - Official website of the Clojure programming language.
- [Clojure for the Brave and True](https://www.braveclojure.com/) - A comprehensive guide for learning Clojure.
- [ClojureDocs](https://clojuredocs.org/) - A community-driven documentation site for Clojure.