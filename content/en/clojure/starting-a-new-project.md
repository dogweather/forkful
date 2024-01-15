---
title:                "Starting a new project"
html_title:           "Clojure recipe: Starting a new project"
simple_title:         "Starting a new project"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Why

Are you looking to start a new project in Clojure? Maybe you're a seasoned developer looking for a new challenge, or maybe you're new to the language and curious to learn more. Either way, starting a new project in Clojure can open up opportunities for efficient and elegant solutions to complex problems.

## How To

First, make sure you have the latest version of Clojure installed. Then, create a new project by running the following command in your terminal:

```Clojure
lein new <project-name>
```

This will generate a basic project structure for you. Next, navigate to the newly created project directory and open the `project.clj` file. This is where you can specify any dependencies your project may need.

To add dependencies, you can use the `:dependencies` key in the `project.clj` file like this:

```Clojure
:dependencies [[org.clojure/clojure "1.10.0"]
               [org.clojure/tools.logging "0.4.1"]]
```

Next, create a new Clojure source file in the `src` directory. This is where you can write your code and define functions. For example:

```Clojure
(ns <project-name>.core
  (:require [clojure.tools.logging :as log]))

(defn greet [name]
  (log/info (str "Hello, " name "!")))
```

Finally, to run your program, navigate back to the project directory in your terminal and run the following command:

```Clojure
lein run -m <project-name>.core
```

This will execute your `greet` function and output "Hello, [name]!" in the terminal, where `[name]` is the argument you pass in.

Congratulations, you now have a basic Clojure project up and running!

## Deep Dive

Starting a new project in Clojure also means getting familiar with its unique features, such as its powerful and expressive syntax, functional programming capabilities, and seamless integration with Java libraries. It's also worth exploring the various tools and libraries available within the Clojure ecosystem, such as Leiningen, a build and dependency management tool, and ClojureScript, which allows you to write Clojure code that compiles to JavaScript.

Additionally, it's important to familiarize yourself with the standard Clojure library and core functions, which provide a wide range of useful and efficient functions for data manipulation, concurrency, and more.

See Also

- Official Clojure website: https://clojure.org/
- ClojureDocs: https://clojuredocs.org/
- Leiningen: https://leiningen.org/
- ClojureScript: https://clojurescript.org/