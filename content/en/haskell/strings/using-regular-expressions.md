---
date: 2024-02-03 19:02:43.421953-07:00
description: "Regular expressions in programming are sequences of characters that\
  \ define a search pattern, typically employed for string searching and manipulation.\u2026"
lastmod: '2024-03-13T22:45:00.119037-06:00'
model: gpt-4-0125-preview
summary: "Regular expressions in programming are sequences of characters that define\
  \ a search pattern, typically employed for string searching and manipulation.\u2026"
title: Using regular expressions
---

{{< edit_this_page >}}

## What & Why?
Regular expressions in programming are sequences of characters that define a search pattern, typically employed for string searching and manipulation. Haskell programmers utilize regular expressions for tasks ranging from simple string matching to complex text processing, capitalizing on their efficiency and versatility in dealing with text data.

## How to:
In Haskell, regex functionalities are not part of the standard library, necessitating the use of third-party packages like `regex-base` along with a compatible backend like `regex-posix` (for POSIX regex support), `regex-pcre` (for Perl-compatible regex), etc. Here's how you can use these packages to work with regular expressions.

First, ensure you have the packages installed by adding `regex-posix` or `regex-pcre` to your project's `.cabal` file or installing via cabal directly:

```bash
cabal install regex-posix
```
or
```bash
cabal install regex-pcre
```

### Using `regex-posix`:

```haskell
import Text.Regex.Posix ((=~))

-- Check if a string matches a pattern
isMatch :: String -> String -> Bool
isMatch text pattern = text =~ pattern :: Bool

-- Find the first match
findFirst :: String -> String -> String
findFirst text pattern = text =~ pattern :: String

main :: IO ()
main = do
    print $ isMatch "hello world" "wo"
    -- Output: True
    print $ findFirst "good morning, good night" "good"
    -- Output: "good"
```

### Using `regex-pcre`:

```haskell
import Text.Regex.PCRE ((=~))

-- Find all matches
findAll :: String -> String -> [String]
findAll text pattern = text =~ pattern :: [String]

main :: IO ()
main = do
    print $ findAll "test1 test2 test3" "\\btest[0-9]\\b"
    -- Output: ["test1","test2","test3"]
```

Each library has its particularities, but the general methodology of using `=~` to apply the regex remains consistent, whether checking for a match or extracting substrings. Choosing between `regex-posix` or `regex-pcre` largely depends on your project's needs and the specific regex capabilities required.
