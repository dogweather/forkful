---
title:                "Haskell recipe: Using regular expressions"
programming_language: "Haskell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Why
Regular expressions are powerful tools used in programming to search, match, and manipulate strings of text. They allow us to quickly and efficiently perform tasks such as validating user input, extracting information from files, and formatting data. Using regular expressions can save time and effort when working with text data, making them a valuable skill for any programmer.

## How To
Using regular expressions in Haskell is simple and straightforward. First, we need to import the `Text.Regex` library, which provides functions for working with regular expressions. Then, we can use the `=~` operator to apply a regular expression to a string and return a Boolean value if it matches.

Let's take a look at a simple example. Suppose we have a string `myString = "Hello, world!"` and we want to check if it contains the word "world". We can use the following code:

```Haskell
import Text.Regex

myString = "Hello, world!"

print (myString =~ "world" :: Bool)
```

The output of this code would be `True`, since the regex found a match for "world" in the string. We can also use regular expressions to extract information from a string using the `=~` operator with capture groups. For example, if we have a string `address = "123 Main St, Anytown, USA"`, we can use the following code to extract the street number and name:

```Haskell
import Text.Regex

address = "123 Main St, Anytown, USA"

print (address =~ "([0-9]+) (.*)" :: (String, String))
```

The first element of the tuple returned by `=~` will be the entire match, while the second element will be the first capture group (in this case, the street number). We can also use capture groups to replace text in a string using the `subRegex` function.

## Deep Dive
Haskell's `Text.Regex` library provides many useful functions for working with regular expressions. Some key functions to keep in mind are `matchRegex`, which returns a list of all matches for a given pattern, and `splitRegex`, which splits a string into a list based on a delimiter specified by a regular expression.

Additionally, we can use special characters in our regular expressions to specify patterns, such as `*` for zero or more repetitions, `+` for one or more repetitions, and `?` for zero or one repetitions. We can also use character sets, such as `[a-z]` to specify any lowercase letter, and `\d` to specify any digit.

It's important to note that regular expressions can be powerful, but they can also be complex and difficult to read. It's always a good idea to test your regular expressions thoroughly and use comments to explain their purpose.

## See Also
- Official Haskell Documentation for Regular Expressions: https://wiki.haskell.org/Regular_expressions
- Regex Tutorial for Beginners: https://www.regular-expressions.info/tutorial.html
- Regex Cheatsheet: https://www.rexegg.com/regex-quickstart.html