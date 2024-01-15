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

## Why
If you're new to Clojure or just starting to explore it, you may have come across the term "command line arguments" and wondered what they are and why they are important. Command line arguments are simply values that are passed to a program when it is executed on the command line. In this article, we will take a look at how to read and use command line arguments in your Clojure programs.

## How To
To read command line arguments in Clojure, we can use the `command-line-args` function from the `clojure.core` namespace. This function returns a sequence of strings, with each string representing a command line argument.

Let's take a look at a simple example:

```Clojure
(ns command-line-example.core
  (:gen-class))

(defn -main []
  (println (command-line-args)))
```

If we run this program from the command line by typing `clj -m command-line-example.core arg1 arg2`, the output would be `("arg1" "arg2")`. We can now use these arguments in our program as needed.

We can also use destructuring to assign specific command line arguments to variables:

```Clojure
(ns command-line-example.core
  (:gen-class))

(defn -main [& args]
  (let [[name age] args]
    (println "Hello" name ", you are" age "years old!")))
```

In this example, we are using destructuring to assign the first two command line arguments to the variables "name" and "age". So if we run this program with `clj -m command-line-example.core John 25`, the output would be `Hello John, you are 25 years old!`. This makes it easier to access and use specific command line arguments in our program.

## Deep Dive
When a program is executed on the command line, any arguments given after the program name are treated as command line arguments. These can be used to specify certain options, settings, or inputs for the program. For example, in a file conversion program, we could pass in the names of the input and output files as command line arguments.

It's important to note that command line arguments are always passed as strings, even if they are numbers or other data types. It is the responsibility of the programmer to convert these strings to the desired data type before using them in the program.

Additionally, Clojure also allows for the use of flags or options as command line arguments. These are typically preceded by a dash and can be used to specify different behaviors or options for the program. For example, `-h` could be used to display a help menu or `-v` to print the program's version number.

## See Also
- [Official Clojure documentation on command line arguments](https://clojure.org/reference/repl_and_main#_command_line_arguments)
- [Clojure from the ground up: Running command-line programs](https://aphyr.com/posts/316-clojure-from-the-ground-up-running)
- [Clojure CLI tools](https://clojure.org/guides/getting_started#_clojure_installer_and_cli_tools)