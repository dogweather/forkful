---
title:                "Haskell recipe: Using regular expressions"
simple_title:         "Using regular expressions"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Why Regular Expressions are Useful in Haskell Programming

Regular expressions are powerful tools that allow for efficient string manipulation and pattern matching in Haskell. They provide a concise and flexible way to search for and manipulate text in a given dataset.

## How To Use Regular Expressions in Haskell

Before we dive into coding examples, it's important to note that in Haskell, regular expressions are implemented using the `Text.Regex` library. This library provides functions that allow us to create and match regular expressions. Let's take a look at some examples of how to use regular expressions in Haskell:

```Haskell
import Text.Regex
```

### Creating Regular Expressions

To create a regular expression in Haskell, we use the `mkRegex` function and pass in a string representation of the regex. For example, we could create a regex to match any word that starts with "hello" by using the following code:

```Haskell
helloRegex = mkRegex "^hello\\w+"
```

### Matching Regular Expressions

After creating a regex, we can use the `matchRegex` function to match it against a given string. This function returns a `Maybe (String, String, String)` where the first element is the full match, the second element is the substring that matched the regex, and the third element is the remaining string.

Let's see an example of how this would work:

```Haskell
-- We will be using this string for all our examples
testString = "hello world, my name is Jane."

-- Match the regex against our test string
-- Note that we need to import the Maybe module
regexMatch = matchRegex helloRegex testString
-- Output: Just ("hello world", "hello", " world, my name is Jane.")

-- Match the regex again to get the substring that matched
matchedSubstring = maybe "" (\(_, m, _) -> m) regexMatch
-- Output: "hello world"

-- Match the regex again to get the remaining string
remainingString = maybe "" (\(_, _, r) -> r) regexMatch
-- Output: " world, my name is Jane."
```

### Using Regular Expressions with Functions

We can also use regular expressions within higher-order functions such as `filter` and `map`. For example, let's say we have a list of names and we want to filter out all the names that start with the letter "J". We can do this using a regular expression and the `filter` function:

```Haskell
namesList = ["John", "Jane", "Mary", "Jenna"]
-- Create a regex to match names starting with "J"
jNamesRegex = mkRegex "^J"
-- Use this regex with the filter function
filteredNames = filter (isJust . matchRegex jNamesRegex) namesList
-- Output: ["John", "Jane", "Jenna"]
```

## Deep Dive into Regular Expressions

Regular expressions are made up of a combination of special characters and normal characters that define a pattern to match against a string. Here are some of the most commonly used special characters in regular expressions:

- `^` Matches the beginning of a string
- `$` Matches the end of a string
- `.` Matches any single character
- `*` Matches zero or more occurrences of the previous character
- `+` Matches one or more occurrences of the previous character
- `?` Matches zero or one occurrence of the previous character
- `[ ]` Matches any character within the brackets
- `[^ ]` Matches any character not within the brackets
- `\d` Matches a single digit
- `\w` Matches a single word character (letters, numbers, underscore)
- `\s` Matches a single whitespace character

For a more comprehensive guide on using regular expressions in Haskell, check out the [Haskell Wiki](https://wiki.haskell.org/Regular_expressions).

## See Also

- [Hackage documentation for Text.Regex](http://hackage.haskell.org/package/regex/docs/Text-Regex.html)
- [Regex tutorial by Learn You a Haskell](http://learnyouahaskell.com/starting-out#regular-expressions)
- [Introduction to Regular Expressions in Haskell](https://dzone.com/articles/haskell-first-experiment-regular-expressions)