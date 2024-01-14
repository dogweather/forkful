---
title:    "Haskell recipe: Starting a new project"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Why
Starting a new project can be an exciting and fulfilling experience for any programmer. It allows you to explore new ideas, develop new skills, and create something that has never been done before. Whether you are a beginner or an experienced Haskell programmer, starting a new project can be a great way to challenge yourself and improve your coding abilities.

## How To
To get started on your new Haskell project, first make sure you have the Glasgow Haskell Compiler (GHC) and the Cabal build tool installed on your system. Once you have those set up, create a new directory for your project and navigate to it using the command line.

```Haskell
mkdir myProject
cd myProject
```

Next, initialize your project using Cabal by running:

```Haskell
cabal init
```

You will be prompted to answer a few questions about your project, such as the name, version, and author. Once you have answered these, Cabal will generate a .cabal file, which is used to build and configure your project.

Now it's time to start writing some code! Let's create a new file called "myProject.hs" and open it in your preferred text editor. In this file, we will define a simple function that takes in two integers and returns their sum.

```Haskell
-- Function to add two integers
add :: Int -> Int -> Int
add x y = x + y
```

Save the file and return to the command line. We can now use the GHC compiler to build our project by running:

```Haskell
ghc -o myProject myProject.hs
```

This will generate an executable file called "myProject", which we can run using:

```Haskell
./myProject
```

When prompted, enter two integers and the program will output their sum. Congratulations, you have successfully started your new Haskell project!

## Deep Dive
When starting a new project in Haskell, it's important to think about the structure and organization of your code. The best way to do this is by breaking your project into smaller modules, with each module focusing on a specific task or functionality. This not only makes your code more organized and easier to debug, but it also allows for better code reuse and maintainability.

Another aspect to consider when starting a new project is the use of third-party libraries. Haskell has a vast collection of libraries available for various purposes, and it's always a good idea to explore these when starting a new project. This can save you time and effort in writing complex functions and algorithms, as well as ensuring efficient and optimized code.

Lastly, don't be afraid to ask for help! Haskell has a strong and supportive community, and there are many forums, blogs, and resources available for beginners and experienced programmers alike. Don't hesitate to reach out and ask for guidance or feedback on your project.

## See Also
- [Glasgow Haskell Compiler](https://www.haskell.org/ghc/)
- [Cabal Build Tool](https://www.haskell.org/cabal/)
- [Haskell Libraries](https://hackage.haskell.org/)