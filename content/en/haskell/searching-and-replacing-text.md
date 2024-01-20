---
title:                "Searching and replacing text"
html_title:           "Arduino recipe: Searching and replacing text"
simple_title:         "Searching and replacing text"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Haskell: The Ins and Outs of Searching & Replacing Text

## What & Why?

Searching and replacing text means finding given sequences of characters (strings) within a larger string and swapping them out for other sequences. This operation is core to text processing, making it invaluable to programmers when manipulating data, performing transformations, or removing unwanted characters.

## How to:

Haskell offers inbuilt string functions, but we'll leverage the `Data.Text` library's `replace` function for this exercise. Install the `text` library if you haven't.

```Haskell
import Data.Text (replace, pack, unpack)

searchAndReplace :: String -> String -> String -> String
searchAndReplace old new text = unpack $ replace (pack old) (pack new) (pack text)
```

This function works by converting `String` values to `Text`, applying `replace`, then converting back to `String`. 

Let's give it a spin:

```Haskell
main :: IO ()
main = do
    let old = "banana"
    let new = "apple"
    let text = "I love bananas. Do you love bananas too?"
    putStrLn (searchAndReplace old new text)
```

This script will output:

```Haskell
"I love apples. Do you love apples too?"
```

Bingo! We've replaced all occurrences of "banana" with "apple".

## Deep Dive

Haskell's historical favor for list processing reflects in its core string handling, treating strings as lists of characters. However, this approach isn't ideal for larger applications due to inefficiency. The Haskell `text` library, enacted in 2009, offers high-performance, memory-efficient string processing.

Alternatives include the `stringsearch` library, providing more advanced capabilities like Boyer-Moore and Knuth-Morris-Pratt algorithms. Though `text` is perfectly sufficient for most basic operations and keeps code clean by resembling standard string functions.

While `replace` seems straightforward, it performs multiple transformations: `pack` converts `String` to `Text`, `replace` does the swapping, and `unpack` converts back to `String`.

## See Also

* [Haskell Text Library](https://hackage.haskell.org/package/text)
* [Intro To Haskell](https://www.futurelearn.com/info/courses/functional-programming-haskell/0/steps/27233)
* [Haskell stringsearch Library](https://hackage.haskell.org/package/stringsearch)