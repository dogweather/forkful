---
title:    "Clojure recipe: Reading command line arguments"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Why

One of the fundamental ways of interacting with a program is through the command line. Being able to pass arguments to a program allows for a more dynamic and customizable experience for the user.

## How To

Reading command line arguments in Clojure is a simple but powerful task. All command line arguments are passed to the `clojure.main` function as a vector of strings in the `*command-line-args*` variable.

```Clojure
(defn main 
  [& args]
  (println "Hello" (first args))) ; Passing the first argument to the println function
```
**Output:** `Hello [first argument]`

Arguments can also be passed directly to a specific namespace and function using the `-m` flag.

```Clojure
(ns my-app.core
  (:gen-class))

(defn -main 
  [& args]
  (println "Hello" (first args)))

```

**Command Line:** `clj -m my-app.core [argument]`

**Output:** `Hello [argument]`

## Deep Dive

The `clojure.main` function is the entry point for all command line arguments. It also provides the option to load a specific namespace and function using the `-m` flag, as shown in the previous example. However, for more complex scenarios, the `tools.cli` library can be used to handle command line arguments.

The `tools.cli` library allows for the creation of customizable and flexible command line interfaces. It provides options for specifying required and optional arguments, setting default values, and parsing flags and options. Below is a simple example using `tools.cli`.

```Clojure
(ns my-app.core
  (:require [clojure.tools.cli :refer [cli]]))

(defn -main 
  [& args]
  (let [[options args banner]
        (cli args
          ["-n" "--name NAME" "Your name"]
          ["-a" "--age AGE" "Your age (default: 24)"])]
    (printf "Hello, my name is %s and I am %d years old!" 
            (:name options)
            (or (:age options) 24))))

```

**Command Line:** `clj -m my-app.core -n John -a 30`

**Output:** `Hello, my name is John and I am 30 years old!`

## See Also

- [Official Clojure Documentation](https://clojure.org/reference/repl_and_main#_command_line_arguments)
- [Clojure Docs: tools.cli](https://clojuredocs.org/clojure.tools.cli)
- [Clojure CLI Tools Tutorial](https://github.com/clojure/tools.cli#clojure-cli-tools-tutorial)