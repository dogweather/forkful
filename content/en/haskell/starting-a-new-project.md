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

## Why
Programming in Haskell is a unique and enjoyable experience. Its functional and declarative nature allows for concise and elegant code that is also highly maintainable. Starting a new project in Haskell can not only expand your programming skills, but also lead to more efficient and robust code.

## How To
To start a new project in Haskell, you'll first need to make sure you have the Haskell compiler, GHC, installed on your computer. Once you have that set up, you can follow these simple steps to get your project up and running:

1. Create a new directory for your project.
2. Navigate to that directory in your terminal.
3. Use the `cabal init` command to initialize a new Cabal project.
4. Fill in the prompted fields, including the project name, version, and description.
5. Once the project is initialized, you can use the `cabal build` command to build your project.
6. To run your project, use the `cabal run` command followed by the name of the executable.
7. After making changes to your code, you can use the `cabal build` command again to update your project and run it again with `cabal run`.

Here's an example of a simple "Hello World" program in Haskell:

```Haskell
-- hello.hs
main :: IO ()
main = putStrLn "Hello, world!"
```

Running this program with `cabal run hello` will output `Hello, world!`.

## Deep Dive
When starting a new project in Haskell, it is important to think carefully about your project structure and design. Haskell's strong type system allows for writing robust and bug-free code, but it's important to plan out your types and functions beforehand.

A good starting point is to identify the different modules and their respective functionality in your project. Then, think about the types and functions needed for each module and how they will interact with each other. It may also be helpful to create a visual diagram to map out the relationships between different parts of your project.

Lastly, don't be afraid to consult Haskell documentation and reach out to the Haskell community for help and guidance. The Haskell community is known for being welcoming and supportive, so take advantage of their expertise and knowledge.

## See Also
- Official Haskell website: https://www.haskell.org/
- Learn You a Haskell tutorial: http://learnyouahaskell.com/
- Haskell subreddit: https://www.reddit.com/r/haskell/