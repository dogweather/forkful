---
title:                "Using regular expressions"
html_title:           "Bash recipe: Using regular expressions"
simple_title:         "Using regular expressions"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?
Regular expressions (regex) search and manipulate strings based on patterns. Programmers use them for tasks like form validation, parsing, and text processing because they're powerful and concise.

## How to:
In Haskell, you can use regex with the `regex-tdfa` package. Here, we grab numbers from a string.

```Haskell
import Text.Regex.TDFA ((=~))

main :: IO ()
main = do
  let text = "Order 531 has 2 items"
  let numbers = text =~ "[0-9]+" :: [String]
  print numbers
```

Output:
```
["531","2"]
```

To replace text, you can use `subRegex` from `regex-compat`.

```Haskell
import Text.Regex (subRegex, mkRegex)

main :: IO ()
main = do
  let text = "Hello, 2023!"
  let regex = mkRegex "[0-9]+"
  let newText = subRegex regex text "YEAR"
  putStrLn newText
```

Output:
```
Hello, YEAR!
```

## Deep Dive
Regular expressions date back to the 1950s, conceptualized by mathematician Stephen Kleene. While Haskell was later to the game, it now has a rich set of regex libraries like `regex-tdfa` for POSIX regex, and `regex-pcre` for Perl compatibility. Alternatives to regex include parser combinator libraries like `parsec`, which can offer more readability and maintainability. Regex's in Haskell are not built into the language syntax but are provided through these libraries.

## See Also
- Hackage libraries:
  - regex-tdfa: http://hackage.haskell.org/package/regex-tdfa
  - regex-compat: http://hackage.haskell.org/package/regex-compat
  - regex-pcre: http://hackage.haskell.org/package/regex-pcre
- The Haskell Wiki on regular expressions: https://wiki.haskell.org/Regular_expressions
- "Real World Haskell" by Bryan O'Sullivan, Don Stewart, and John Goerzen for in-depth treatment: http://book.realworldhaskell.org/