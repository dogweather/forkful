---
title:                "Haskell recipe: Finding the length of a string"
simple_title:         "Finding the length of a string"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Why 

As programmers, we often encounter situations where we need to manipulate strings. Whether it's for data processing, user input validation, or simply displaying text, strings play a crucial role in programming. And one of the most basic operations on strings is finding their length. In this blog post, we will explore how to find the length of a string in Haskell and why it is an important skill for any programmer to have.

## How To 

Finding the length of a string in Haskell is a simple yet powerful operation. In Haskell, strings are represented as lists of characters. Therefore, finding the length of a string is essentially finding the length of this list. Let's take a look at the code below to see how we can do this:

```Haskell
-- Define a string
myString = "Hello World!"

-- Use the standard library function 'length' to find the length of the string
lengthOfMyString = length myString

-- Display the result
print lengthOfMyString
```

The above code will output the length of the string, which in this case is 12. We can also use the `length` function to find the length of a string inputted by the user. Let's take a look at another example:

```Haskell
-- Prompt the user to enter a string
putStrLn "Enter a string: "

-- Read the input and store it in a variable
userString <- getLine

-- Find the length of the string
lengthOfUserString = length userString

-- Display the result
print lengthOfUserString
```

Now the code will output the length of the string entered by the user. Go ahead and try it out yourself!

## Deep Dive 

Now that we know how to find the length of a string in Haskell, let's take a deep dive into how it actually works. The `length` function is defined as `length :: [a] -> Int` in the standard library. It takes in a list of any type `a` and returns an `Int`, which represents the length of the list.

Haskell uses lazy evaluation, which means that it will only evaluate the expression when it is needed. This is why the `length` function works efficiently even with large strings.

It is also important to note that this function does not count the empty string as a character, unlike many other programming languages. This is because in Haskell, the empty string is represented as an empty list, `[]`.

## See Also 

- [Learn You a Haskell for Great Good](http://learnyouahaskell.com/chapters)
- [Haskell Wiki](https://wiki.haskell.org/Learning_Haskell)
- [Haskell Language Tutorial](https://www.tutorialspoint.com/haskell/index.htm)

Give it a try and incorporate finding the length of a string in your next Haskell program. Happy coding!