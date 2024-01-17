---
title:                "Starting a new project"
html_title:           "Haskell recipe: Starting a new project"
simple_title:         "Starting a new project"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/starting-a-new-project.md"
---

{{< edit_this_page >}}

## What & Why?

Starting a new project in Haskell means creating a new software project from scratch using the Haskell programming language. Programmers often start a new project to solve a specific problem or create a new software product. 

## How to:

To start a new project in Haskell, you will need to follow these steps:

1. Open a command line interface on your computer.
2. Navigate to the directory where you want to create your project.
3. Type `stack new project-name` and hit enter.
4. This will create a new directory named `project-name` with the necessary files and folder structure for a Haskell project.
5. Next, cd into the project directory by typing `cd project-name` and hit enter.
6. Then, run `stack build` to build your project.
7. Finally, you can run your project by typing `stack exec project-name` and hit enter.

Here is an example of creating and running a project named "my-haskell-project":

```Haskell
$ stack new my-haskell-project
$ cd my-haskell-project
$ stack build
$ stack exec my-haskell-project
```

This will build and run your project, displaying any output in the command line interface.

## Deep Dive:

Haskell is a purely functional programming language that was first introduced in 1990. Unlike other programming languages, Haskell is based on mathematical principles and has a strong type system. This allows for robust and concise code, making it a popular choice for writing reliable and scalable software.

There are alternative ways to start a new project in Haskell, such as using a package manager like Cabal or creating a new project in an integrated development environment (IDE) like Visual Studio Code. However, using the Haskell-specific tool, Stack, is the recommended method as it handles dependencies and build management efficiently.

Creating a new project using Stack also gives you access to a wide range of libraries and packages from the Hackage repository, making it easier to add additional functionality to your project.

## See Also:

If you want to learn more about starting a new project in Haskell, or want to explore other aspects of the language, check out the following resources:

- [Haskell Programming Language Official Website](https://www.haskell.org/) - The official website for the Haskell programming language, with documentation, tutorials, and community resources.
- [Haskell Wiki](https://wiki.haskell.org/) - A community-maintained wiki with extensive information on Haskell and related tools.
- [Learn You a Haskell for Great Good!](http://learnyouahaskell.com/) - A popular online book for learning Haskell from the basics to advanced concepts.