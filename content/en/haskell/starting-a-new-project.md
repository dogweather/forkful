---
title:                "Haskell recipe: Starting a new project"
programming_language: "Haskell"
category:             "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Why

If you're interested in functional programming and want to challenge yourself with a new language, starting a project in Haskell can be an exciting and rewarding experience. Haskell is a purely functional language that allows you to write elegant, concise code that is also highly efficient.

## How To

To get started with Haskell, you'll need to download a Haskell compiler, such as GHC (Glasgow Haskell Compiler). Once you have that installed, you can use a text editor or integrated development environment (IDE) to write your code. Let's look at a simple example of a function that calculates the area of a rectangle:

```Haskell
area :: Double -> Double -> Double
area x y = x * y
```

Here, we've declared a function called "area" that takes in two parameters, "x" and "y", both of type "Double". The function multiplies the two parameters together to calculate the area and returns a value of type "Double". Let's try running this function in the GHCi (GHC interactive) tool:

```Haskell
Prelude> area 5 6
30.0
```

As you can see, we specified the values for "x" and "y" when calling the function, and it correctly calculated the area of a rectangle with sides of length 5 and 6.

Haskell also has powerful pattern matching capabilities, which allow you to define different cases for a given function. For example:

```Haskell
sayHello :: String -> String
sayHello "World" = "Hello, World!"
sayHello name = "Hello, " ++ name ++ "!"
```

Here, we have a function called "sayHello" that takes in a string parameter and returns a string. The first case specifies that if the parameter is equal to "World" then the function will return "Hello, World!" Otherwise, it will concatenate the name with "Hello, " and "!" to create a personalized greeting.

## Deep Dive

Functional programming is heavily reliant on recursion, and Haskell is no exception. Recursion is the process of a function calling itself until a base case is reached. This allows for elegant and concise solutions to various problems, such as calculating factorials or creating a Fibonacci sequence.

Haskell also has a strong type system that helps catch errors at compile time, making it easier to write bug-free code. It also allows for the creation of user-defined data types, which can help in modeling real-world problems.

When starting a project in Haskell, it's essential to understand the concept of "monads," which are used for managing side effects and keeping code pure. They can be a challenging but crucial aspect of learning Haskell.

## See Also

Here are some useful resources to help you get started with Haskell:

- [Haskell Official Website](https://www.haskell.org/)
- [Learn You a Haskell for Great Good!](http://learnyouahaskell.com/)
- [Real World Haskell](http://book.realworldhaskell.org/)
- [Haskell Wiki](https://wiki.haskell.org/)

Now that you have an overview of why and how to start a project in Haskell, go ahead and give it a try! Happy coding!