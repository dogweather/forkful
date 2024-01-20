---
title:                "Reading command line arguments"
html_title:           "C++ recipe: Reading command line arguments"
simple_title:         "Reading command line arguments"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why?
In programming, reading command line arguments means fetching inputs given directly via the interface at program launch. It allows users to run different configurations of your program without modifying code.

## How to: 
Here's how you read command line arguments in Clojure:

```Clojure
(defn -main [& args]
  (println "Command line args:" args))
```

Run this in your terminal:

```shell
$ clojure my-program.clj arg1 arg2 arg3
```

You'll see:

```shell
Command line args: (arg1 arg2 arg3)
```

## Deep Dive
Historically, command-line interaction predates graphical user interfaces, offering programmers direct interaction with the system. In Clojure, `-main` function serves as the primary entry point to a Clojure application when launched from the command line.

Alternatives to read command line args in Clojure include using java interop like this: 

```Clojure
(. (System/getProperties) get "sun.java.command"))
```

However, Clojure's var-args (`& args`) in `-main` keep things simple & idiomatic.

Command-line args are passed as strings. Consider parsing or validating them before use. If you have heaps of args, consider libraries like `tools.cli` to help keep things manageable.

## See Also
- Clojure `-main` function usage: [https://clojure.org/reference/compilation#_the_dot_main_dot_function](https://clojure.org/reference/compilation#_the_dot_main_dot_function)
- Tools.cli, for complex arg handling: [https://github.com/clojure/tools.cli](https://github.com/clojure/tools.cli)