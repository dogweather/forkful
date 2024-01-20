---
title:                "Extracting substrings"
html_title:           "Arduino recipe: Extracting substrings"
simple_title:         "Extracting substrings"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/extracting-substrings.md"
---

{{< edit_this_page >}}

# Haskelling Substrings: Extraction Basics

## What & Why?

"Extracting substrings" entails taking a smaller string from within a larger one. It's a common programming necessity: manipulating identifiers, filtering input, parsing language, the list goes on.

## How to:

In Haskell, `take` and `drop` are your go-to for substring manipulation. Their combination results in very clean, clear substring extraction. 

Here, `take n str` returns the first `n` characters, `drop n str` the remaining. Look:

```Haskell
let str = "Hello, World!"
take 5 str    -- output: "Hello"
drop 7 str    -- output: "World!"
```

"Wanna slice 'n dice within, say, a string's middle? Meet our friend `drop` again:

```Haskell
let str = "Extract some substring"
drop 8 (take 12 str)    -- output: "some"
```

Drop the first 8, take 12 total, the result? The substring "some". Simple? Simple. Elegant? Absolutely.

## Deep Dive

Substring extraction goes way back to the inception of computer programming, being integral to programming languages from COBOL to JavaScript.

There are alternatives, yes. `splitAt` gives you a tuple with the first half (up to the point of split) and second half of a string.

```Haskell
splitAt 5 "Hello, World!"    -- output: ("Hello",", World!")
```

They all essentially use the 'List' implementation within Haskell because, within this lingo, a string is a list of characters.

## See Also

1. `take` and `drop` in Haskell's official documentation [here](https://hackage.haskell.org/package/base-4.15.0.0/docs/Prelude.html#g:4).


3. Alternative approaches, including Haskell Libraries for regex pattern matching (like Text.Regex) [here](https://wiki.haskell.org/Regular_expressions).

Dive in, and let Haskell's simplicity and power elevate your substring game to the next level.