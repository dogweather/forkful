---
title:                "Starting a new project"
html_title:           "Bash recipe: Starting a new project"
simple_title:         "Starting a new project"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/starting-a-new-project.md"
---

{{< edit_this_page >}}

## What & Why?

Starting a new project in Haskell involves initiating a fresh, practical context for software development. This process is essential as it paves the way for structured and efficient code-making, streamlining the execution of ideas.

## How to:

To start with, let's initiate a new project using Stack. Stack is a cross-platform Haskell development tool.

```Haskell
stack new myProject
```

This command builds a new project named `myProject`. When you run this in your terminal, Stack will create a folder with the same name, containing default source files and other necessary configurations.

To build the project, navigate into the created directory and use this command:

```Haskell
cd myProject
stack build
```

And, to run the project:

```Haskell
stack exec myProject-exe
```

You should see the phrase `someFunc` in your console. `someFunc` is the main function run by the `Main.hs` file created by the `stack new` command.

## Deep Dive

Haskell, being an advanced purely-functional programming language, has a sound historic lineage, inspired by the mathematics of the Lambda Calculus. Starting a new project in Haskell is a thoughtful choice as it allows for efficient, clean, and predictable coding.

While 'stack' is an excellent choice for Haskell, there are alternatives like Cabal and Nix to consider. Cabal is versatile and older but comes with a learning curve. Nix provides a more advanced, reliable, and reproducible environment for Haskell projects but may come across as complex for beginners.

Staring a new project with Stack in Haskell is about setting up an isolated environment that carries its dependencies, promoting reproducible builds. It creates a basic directory structure: an application directory, a test directory, and a `package.yaml` along with other files. The directory and file structures play a significant role in organizing your code development and maintenance.

## See Also

1. [Learn You a Haskell for Great Good!](http://learnyouahaskell.com/chapters): A great online resource to get started with Haskell.
2. [Real World Haskell](http://book.realworldhaskell.org/): A comprehensive book on Haskell with real-world examples.
3. [Stack Documentation](https://docs.haskellstack.org/): For a deeper dive into Stack.
4. [Nix Documentation](https://nixos.wiki/wiki/Nix): To get started with Nix.
5. [The Cabal User Guide](https://cabal.readthedocs.io/): To understand more about Cabal.
6. [What I Wish I Knew When Learning Haskell](http://dev.stephendiehl.com/hask/): A complete understanding of Haskell with concepts and exercises.