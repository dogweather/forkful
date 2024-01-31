---
title:                "Removing quotes from a string"
date:                  2024-01-25T20:50:36.339014-07:00
model:                 gpt-4-1106-preview
simple_title:         "Removing quotes from a string"

category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Removing quotes from a string means stripping out any quotation marks—single (' ') or double (" ")—that are part of the string data. Programmers do it to sanitize inputs, prepare text for processing, or get rid of unnecessary characters that might interfere with data handling and operations.

## How to:
In Haskell, we can whip up a function that removes all quotes from a given string. It's like telling the quotes to scram, and making sure they take the hint.

```Haskell
import Data.List (intercalate)
import Data.Char (isPunctuation)

removeQuotes :: String -> String
removeQuotes = filter (\c -> c /= '"' && c /= '\'')

main :: IO ()
main = do
    let stringWithQuotes = "Haskell said, \"Let's learn some functions!\""
    putStrLn $ removeQuotes stringWithQuotes
```

Sample Output:

```
Haskell said, Lets learn some functions!
```

## Deep Dive
Once upon a time, before strings in programming were as common as cat videos on the internet, handling text was a finicky business. But as programming languages evolved, strings became a crucial part of coding. Yet, quotes remained a dual-edged sword—essential for defining strings, but a nuisance when included as actual data.

Alternatives? Instead of swatting away all the quotes like flies, you can be selective. You might want to remove only the outermost quotes (a classic trim) or handle escaped quotes within a string.

Implementation-wise, the `removeQuotes` function above uses a lambda to check each character (`c`) to see if it's a pestering quote and filters them out accordingly. This is a straightforward approach, but for bigger texts or more complex rules, you might want to look at parser libraries like `Parsec` which can give you more finesse and power in text processing.

## See Also:
- For regex lovers: [Text.Regex.Posix](https://hackage.haskell.org/package/regex-posix)
- A gentle intro to Haskell strings: [Learn You a Haskell for Great Good! - Starting Out](http://learnyouahaskell.com/starting-out#strings)
