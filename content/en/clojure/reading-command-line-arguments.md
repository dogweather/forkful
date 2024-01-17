---
title:                "Reading command line arguments"
html_title:           "Clojure recipe: Reading command line arguments"
simple_title:         "Reading command line arguments"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why?

Reading command line arguments is the process of obtaining data that is entered by a user when executing a program through the command line interface. Programmers use this functionality to allow users to customize the behavior of their program, for example by specifying file paths or input parameters.

## How to:

Coding example in Clojure:

```
(defn -main [& args]
    (print (str "Hello, " (first args) "!")))

;; Program execution:
$ java -cp my-program.jar my.program.namespace -name World
Hello, World!
```

## Deep Dive:

Reading command line arguments has been a fundamental feature of programming since the early days of computers. In addition to customizing program behavior, it is also useful for automating tasks through scripts. As an alternative to command line arguments, some programming languages offer graphical user interfaces for user input.

Reading command line arguments in Clojure is accomplished by using the `& args` parameter in the `-main` function. This parameter captures all the arguments passed by the user and stores them in a vector. Alternatively, the `clojure.tools.cli` library can be used for more advanced parsing and validation of command line arguments.

## See Also:

- [Clojure Documentation on Command Line](https://clojure.org/reference/repl_and_main#_command_line)
- [clojure.tools.cli library](https://github.com/clojure/tools.cli)