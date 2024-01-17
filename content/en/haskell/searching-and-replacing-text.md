---
title:                "Searching and replacing text"
html_title:           "Haskell recipe: Searching and replacing text"
simple_title:         "Searching and replacing text"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# What & Why?
Searching and replacing text is a common task in programming that involves finding a specific pattern within a string of characters and replacing it with another pattern. This is often done to modify code or data to fit a desired format or to fix errors. Programmers do it to automate repetitive tasks, save time, and ensure consistency in their code.

# How to:
To search and replace text in Haskell, we can use the `substitute` function from the `Text.Regex.Posix` module. This function takes in a regular expression pattern, the replacement string, and the input string, and returns the modified string.

```Haskell
import Text.Regex.Posix (substitute)

-- Replace all occurrences of "foo" with "bar" in "foobar123"
substitute "foo" "bar" "foobar123"  -- Output: "barbar123"

-- Use a regular expression to replace all non-alphabetic characters with "-"
substitute "[^a-zA-Z]" "-" "abc123def"  -- Output: "abc-def"
```

We can also use the `=~` operator to replace text in a more concise way, by specifying the replacement string in the regular expression itself.

```Haskell
-- Replace all numbers with their respective cubes in "1 2 3"
"1 2 3" =~ "([0-9]+)" :: String  -- Output: "1 8 27"
```

# Deep Dive:
Searching and replacing text has been a fundamental task in programming since the early days of computing, and various methods have been developed over time. In Haskell, the `substitute` function uses POSIX regular expressions, which are powerful tools for string manipulation. However, alternative methods, such as using the `replace` function from the `Data.List.Utils` module or implementing our own custom replace function using recursion, also exist.

Internally, the `substitute` function uses the `Text.Regex.Posix.ByteString` module to handle input as bytestrings, which can be more efficient than working with characters directly. Additionally, Haskell's strong type system ensures that our regular expressions are validated at compile time, reducing the chances of runtime errors.

# See Also:
- [Haskell documentation on searching and replacing text](https://hackage.haskell.org/package/regex-posix-0.96.0.0/docs/Text-Regex-Posix.html#v:substitute)
- [Tutorial on using regular expressions in Haskell](https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/parsing-floats-with-regular-expressions)
- [Alternative string manipulation methods in Haskell](https://hackage.haskell.org/package/base/docs/Data-String.html)