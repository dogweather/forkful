---
date: 2024-01-20 17:38:47.247372-07:00
description: "How to: Haskell uses the `Data.Char` module to manipulate characters.\
  \ The `toLower` function specifically changes a single character to lower case.\
  \ You'll\u2026"
lastmod: '2024-03-13T22:45:00.116268-06:00'
model: gpt-4-1106-preview
summary: Haskell uses the `Data.Char` module to manipulate characters.
title: Converting a string to lower case
weight: 4
---

## How to:
Haskell uses the `Data.Char` module to manipulate characters. The `toLower` function specifically changes a single character to lower case. You'll map this function over a string to convert it entirely to lower case. Check out the code:

```haskell
import Data.Char (toLower)

-- Convert a string to lower case
lowercaseString :: String -> String
lowercaseString = map toLower

-- Usage
main :: IO ()
main = putStrLn $ lowercaseString "Hello, Haskell!"
```

Sample output:

```
hello, haskell!
```

## Deep Dive
Historically, the concept of letter cases comes from the era of manual typesetting when uppercase and lowercase letters were kept in separate cases. In programming, case conversion ensures uniformity, especially in case-insensitive operations.

Here's the DL on Haskell particulars. The `Data.Char` module, housing `toLower`, debuted in the Haskell 98 standard. It's been the go-to for character manipulations ever since. Other languages have their own methods, like `.toLowerCase()` in JavaScript or `.lower()` in Python, but in Haskell, `map` and `toLower` do the trick neatly.

Under the hood, `toLower` considers Unicode, meaning it can handle a vast repertoire of characters and scripts beyond the basic ASCII range – helpful for internationalization.

Alternatives? Sure, you could roll out your own function mimicking `toLower`, but why reinvent the wheel? Stick to `Data.Char` for readability and reliability. Plus, libraries like `text` and `bytestring` offer more performance-tuned approaches if you're working with large datasets or aiming for performance.

## See Also
- `Data.Char` documentation: https://hackage.haskell.org/package/base-4.16.1.0/docs/Data-Char.html
- Haskell 98 Report on `Data.Char`: https://www.haskell.org/onlinereport/standard-prelude.html
- Text library for Haskell: https://hackage.haskell.org/package/text
- ByteString library for Haskell: https://hackage.haskell.org/package/bytestring
