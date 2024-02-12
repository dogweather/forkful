---
title:                "Starting a new project"
aliases:
- en/clojure/starting-a-new-project.md
date:                  2024-01-20T18:03:18.567037-07:00
model:                 gpt-4-1106-preview
simple_title:         "Starting a new project"

tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/starting-a-new-project.md"
---

{{< edit_this_page >}}

## What & Why?

Starting a new project means setting up a fresh programming environment for your code. Programmers do it to kick off development with a clean slate and organize thoughts into tangible code.

## How to:

To bootstrap a Clojure project, we'll use Leiningen, a popular build tool for Clojure:

``` Clojure
;; 1. Install Leiningen if you haven't (https://leiningen.org/)
;; 2. Generate a new project skeleton:
lein new app my-cool-app

;; 3. Navigate into your new project:
cd my-cool-app

;; 4. Start a REPL (Read-Eval-Print Loop):
lein repl

;; Sample output:
;; nREPL server started on port 12345 on host 127.0.0.1 - nrepl://127.0.0.1:12345
;; REPL-y 0.4.4, nREPL 0.6.0
;; Clojure 1.10.1
;; Java 1.8.0_232
;;     Docs: (doc function-name-here)
;;           (find-doc "part-of-name-here")
;;   Source: (source function-name-here)
;;  Javadoc: (javadoc java-object-or-class-here)
;;     Exit: Control+D or (exit) or (quit)
;;  Results: Stored in vars *1, *2, *3, an exception in *e

;; 5. Create a file for your code (src/my_cool_app/core.clj) and open it in your favorite text editor.

;; 6. Write some simple Clojure code:
(ns my-cool-app.core)

(defn say-hello []
  (println "Hello, Clojure world!"))

;; 7. Run your function in the REPL:
(my-cool-app.core/say-hello)

;; Sample output:
;; Hello, Clojure world!
```

## Deep Dive

Clojure projects often start with Leiningen or Boot for managing dependencies, building, and automating tasks. Leiningen has been around since 2010 and has become the default choice for most Clojurists. 

Alternative tools do exist, like `deps.edn` and Clojure CLI tools, which were introduced by Clojure/core to provide more straightforward dependency management and project configuration.

Clojure itself values immutability and functional programming. Starting a project correctly emphasizes clean state management and separation of concerns across functions and namespaces.

Projects typically adhere to a standard directory structure:
- `src/` for your main code.
- `test/` for test code.
- `resources/` for non-code resources.
- `project.clj` or `deps.edn` to manage dependencies and configurations.

A good practice is to keep things minimal at the start. Add dependencies as you go, keeping your project light and manageable.

## See Also

- [Leiningen's Getting Started Guide](https://leiningen.org/#getting-started)
- [Clojure Docs](https://clojuredocs.org/)
- [Clojure Style Guide](https://guide.clojure.style/)
- [Clojure CLI tools](https://clojure.org/guides/getting_started)
- [The Clojure Toolbox](https://www.clojure-toolbox.com/)
