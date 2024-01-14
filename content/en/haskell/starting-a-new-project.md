---
title:                "Haskell recipe: Starting a new project"
simple_title:         "Starting a new project"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Why

Starting a new project in Haskell can be a daunting task for some, but it can also be an exciting and rewarding experience. With a functional programming language like Haskell, you have the opportunity to create highly performant and reliable software that is also easy to maintain and extend. So why not dive into the world of Haskell and see what you can create?

## How To

To get started with Haskell, you will need to have the Haskell platform installed on your computer. Once that is set up, you are ready to start coding! Let's go through a basic example of creating a simple function in Haskell.

```Haskell
-- Define a function called add
add :: Int -> Int -> Int
add x y = x + y  -- add takes in two Integers and returns their sum

-- Use the function
add 2 3
-- Output: 5
```

In this example, we have defined a function called "add" which takes in two Integers and adds them together. We have specified the types of the function parameters and the return type using the arrow notation. Once the function is defined, we can use it by passing in two values and getting back the sum.

## Deep Dive

When starting a new project in Haskell, it is important to plan out your code architecture and data structures beforehand. This will help you stay organized and make the development process smoother. You should also familiarize yourself with the standard library and popular third-party libraries, such as "Data.Text" for working with text data and "HSpec" for testing.

Another important aspect to consider when starting a new project in Haskell is using a build tool, such as "Stack" or "Cabal". These tools can help you manage dependencies, build your project, and run tests.

## See Also

For more resources on getting started with Haskell and building projects, check out the following links:

- [Haskell.org](https://www.haskell.org/) - Official website for Haskell, with tutorials, documentation, and community forums.
- [Learn You a Haskell for Great Good!](http://learnyouahaskell.com/) - A beginner-friendly book on Haskell.
- [Haskell Weekly](https://haskellweekly.news/) - A weekly newsletter with news, job postings, and learning resources.
- [Real World Haskell](http://book.realworldhaskell.org/read/) - A comprehensive guide on using Haskell in real-world projects.