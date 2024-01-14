---
title:    "Haskell recipe: Starting a new project"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/haskell/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Why

Starting a new project in Haskell may seem like a daunting task, but the benefits far outweigh any initial challenges. By using a functional and statically-typed language like Haskell, programmers can create robust and efficient code that is easier to maintain and debug. Additionally, Haskell's strong type system allows for safer and more reliable software, making it a great choice for new projects.

## How To

To get started on a new project in Haskell, first make sure you have the Glasgow Haskell Compiler (GHC) and the Haskell build tool, Cabal, installed on your system. Once those are set up, you can create a new project using the `cabal init` command. This will prompt you for some basic information about your project, such as the name and version number. 

After your project is created, you can begin writing code in the `main.hs` file within the `src` directory. Here's an example of a simple Haskell program that greets the user:

```Haskell
-- main.hs
-- Simple greeting program

main = do
    putStrLn "Hello, what's your name?"
    name <- getLine
    putStrLn $ "Nice to meet you, " ++ name ++ "!"
```

To run this program, navigate to the root directory of your project and use the `runhaskell` command:

```
runhaskell src/main.hs
```

The output should look something like this:
```
Hello, what's your name?
John
Nice to meet you, John!
```

Haskell's syntax can be quite different from other languages, but don't let that discourage you. With practice and patience, you'll quickly get the hang of it.

## Deep Dive

One of the key components of starting a new project in Haskell is choosing and setting up a development environment. The two most popular options are using an integrated development environment (IDE) or a text editor with a Haskell plugin. Whichever you choose, it's important to understand the basic concepts of Haskell, such as functional programming, lazy evaluation, and type inference, to effectively write code.

Another important aspect of starting a new project is choosing the right libraries and dependencies. Haskell has a vast collection of open-source libraries available in the Hackage repository, so you can easily find and use packages to add additional functionality to your project. It's also important to keep your dependencies up-to-date to ensure your project runs smoothly.

Lastly, testing is crucial for any project, and Haskell offers a variety of testing frameworks such as HUnit and QuickCheck. Writing tests early on can help catch bugs and prevent future issues, making it easier to maintain your code in the long run.

## See Also

- Official Haskell website: https://www.haskell.org/
- Haskell for Imperative Programmers: https://www.youtube.com/watch?v=cuTb8Uh1zYw
- Hackage package repository: https://hackage.haskell.org/