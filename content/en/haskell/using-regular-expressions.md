---
title:    "Haskell recipe: Using regular expressions"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Why

Many programmers often find themselves faced with the task of searching and manipulating large amounts of text. Regular expressions, or regex, offer a powerful and efficient solution for this common problem. By learning how to use regex, you can save time and effort in your programming tasks.

## How To

To use regular expressions in Haskell, you will need to import the `Text.Regex` module. Let's take a look at a simple example of matching a pattern in a string, and extracting the matching portion using the `=~` operator:

```Haskell
import Text.Regex

-- Match the first pattern in a string and return the matching group
extractMatch :: String -> Maybe String
extractMatch str = str =~ "[a-z]+" :: Maybe String

main :: IO ()
main = do
  let text = "Hello, World!"
  let result = extractMatch text
  putStrLn $ case result of
    Just match -> "Match found: " ++ match
    Nothing -> "No match found."
```

In this example, we are searching for a pattern of one or more lowercase letters in the given string, and if a match is found, we return that portion of the string. Running this code will produce the following output:

```
Match found: ello
```

The `=~` operator takes in a string to search and a regular expression pattern, and returns a `Maybe` type, which will either be `Just match` if a match is found, or `Nothing` if no match is found.

But regular expressions can do much more than just simple pattern matching. They can also be used for replacing text in a string, splitting a string into parts, and even more complex tasks such as validating email addresses or URLs. The possibilities are endless.

## Deep Dive

Regular expressions follow a specific syntax and have a variety of special characters and metacharacters that can be used to specify patterns. Here are a few examples:

- `.` matches any single character
- `^` matches the beginning of a string
- `$` matches the end of a string
- `*` matches zero or more occurrences of the previous character/pattern
- `+` matches one or more occurrences of the previous character/pattern
- `?` matches zero or one occurrence of the previous character/pattern

For a full list of metacharacters and their meanings, check out the official [Haskell Regex documentation](https://hackage.haskell.org/package/regex-base-0.93.2/docs/Text-Regex.html).

One important thing to note is that regular expressions are case-sensitive by default. To make them case-insensitive, you can use the `(?i)` flag. For example, if we wanted to match both "hello" and "Hello" in our previous example, we could modify the regular expression to `(?i)[a-z]+`.

Regular expressions can also be combined using the `|` (alternate) operator. This allows us to specify multiple patterns to search for, and if any of them match, the entire string will be considered a match.

## See Also

- [Haskell Regex Documentation](https://hackage.haskell.org/package/regex-base-0.93.2/docs/Text-Regex.html)
- [Official Regex Tutorial](https://www.regular-expressions.info/tutorial.html)
- [Haskell Wiki Regex Tutorial](https://wiki.haskell.org/Regular_expressions)