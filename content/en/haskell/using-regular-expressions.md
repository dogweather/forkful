---
title:    "Haskell recipe: Using regular expressions"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/haskell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Why

Regular expressions, commonly known as regex, are powerful tools used for pattern matching and text manipulation in programming languages. They allow for efficient and flexible processing of textual data, making them essential for tasks such as data validation, text parsing, and even text mining. Whether you are a beginner or an experienced programmer, understanding regular expressions can greatly improve your coding skills and make your programs more efficient and effective.

## How To

Using regular expressions in Haskell is made easy with the `regex-applicative` library. To install it, open your terminal and run the command `stack install regex-applicative`. Once installed, import the library in your Haskell file with `import Text.Regex.Applicative`.

To create a regex pattern, use the `RE` data type and its various constructors. For example, to match a string that starts with "Hello" and ends with "world", the pattern would be `RE "Hello.*world"`. The `.*` in the pattern indicates that the string can contain any number of characters in between "Hello" and "world".

To use this pattern for matching, we can use the `match` function from the `Text.Regex.Applicative.Text` module. This function takes in the pattern as the first argument and the string you want to match as the second argument. The output of this function is a `Maybe` type, which can either be `Just` containing the matched string or `Nothing` if the match is unsuccessful.

```Haskell
import Text.Regex.Applicative
import Text.Regex.Applicative.Text

main = do
  let pattern = RE "Hello.*world"
  let string = "Hello there, world!"
  print $ match pattern string
```

The output of this code would be `Just "Hello there, world!"`, indicating a successful match.

## Deep Dive

Regular expressions have a complex syntax that may seem daunting for beginners. Some common patterns include character classes, repetition, and alternation.

Character classes are used to match specific sets of characters. For example, `[0-9]` would match any digit from 0 to 9. This can also be written as `\d` using the escape character `\` to indicate a special character like digits. 
Repetition is denoted by symbols such as `*` for zero or more occurrences, `+` for one or more, and `?` for zero or one. For example, the pattern `RE "ba?d"` would match both "bd" and "bad". 
Alternation is used to match one of multiple patterns. This is denoted by the `|` symbol. For example, the pattern `RE "cat|dog"` would match either "cat" or "dog".

There are many more advanced features and techniques for using regular expressions, such as capturing groups and lookarounds. It is important to practice and experiment with different patterns to fully grasp their capabilities.

## See Also

- [Official Haskell documentation on regular expressions](https://www.haskell.org/onlinereport/standard-prelude.html#t:Regex)
- [Regex tutorial on TutorialsPoint](https://www.tutorialspoint.com/regex/)
- [Video tutorial on regular expressions in Haskell](https://www.youtube.com/watch?v=z88DjFdgEwk)