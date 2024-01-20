---
title:                "Starting a new project"
html_title:           "Bash recipe: Starting a new project"
simple_title:         "Starting a new project"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/starting-a-new-project.md"
---

{{< edit_this_page >}}

## What & Why?
Starting a new project means creating a fresh, neatly organized environment, custom-tailored to solve a specific task or set of tasks. It's vital because it helps programmers stay organized, which aids in code maintenance and team collaboration.

## How to:
Starting a new Clojure project is super simple with Leiningen tool. Here’s how:

First, install Leiningen by following the official guide [here](https://leiningen.org/). 

Once Leiningen is installed, head to your terminal and type the following command:

```Clojure
lein new my-clojure-project
```

This creates a new Clojure project directory named "my-clojure-project". Simple, right? Inside this new directory, you'll find core.clj file in the src/my_clojure_project directory. When we talk about code, this file is where all your genius ideas will be placed. 

To run your new project, navigate into your project directory and type:

```Clojure
lein run
```

If done correctly, you'll see "Hello, World!" pop onto your terminal. Congrats on your new Clojure project!

## Deep Dive
Clojure, a dialect of the Lisp programming language, was created by Rich Hickey to provide a lisp-like language that runs on modern platforms (like Java and JavaScript runtime) and caters to functional programming. 

When it comes to starting a new project, there are alternatives to Leiningen, like Boot and the Clojure command-line tools, but Leiningen remains a firm favorite due to its robustness and ease of use. 

Understanding how Leiningen works under the hood isn’t necessary for starting a new project, but knowledge can be power. Leiningen works by creating a project directory and generating a project.clj file within it. This file is what Leiningen uses to manage your project's dependencies and settings. 

## See Also
Consider exploring these additional resources to reinforce your understanding:
- [Clojure for the Brave and True](https://www.braveclojure.com/)
- [Getting started with Clojure](https://clojure.org/guides/getting_started)