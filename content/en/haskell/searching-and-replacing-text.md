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

## Why

If you've ever had to make a large-scale change in a document or code, you know how tedious it can be to manually search and replace each instance. Luckily, Haskell offers powerful tools for automating this process, making it quick and painless. 

## How To

In Haskell, there are two main functions for searching and replacing text: `substitute` and `replace`.

To use `substitute`, first import the `Data.Text` module. Then, use the function by passing in the text you want to replace, the text you want to replace it with, and the target text.

```
import Data.Text
substitute "Haskell" "Haskell 2021" "I love Haskell"
```

The output will be: `"I love Haskell 2021"`, with "Haskell" substituted for "Haskell 2021".

For `replace`, import the `Data.Text.Replace` module and use the function by passing in the text you want to replace, the text you want to replace it with, and the target text. 

```
import Data.Text.Replace
replace "dog" "cat" "I have a dog named Max"
```

The output will be: `"I have a cat named Max"`, with "dog" replaced with "cat".

## Deep Dive

Both `substitute` and `replace` are powerful functions, but there are a few key differences between them. 

Firstly, `substitute` will only replace the first instance of the target text, while `replace` will replace all instances. Additionally, `substitute` is case-sensitive, while `replace` is case-insensitive.

It's also worth noting that both functions operate on `Text` data types, so any input or output will need to be converted to `Text` using the `pack` function. 

## See Also

- [Haskell Documentation on Data.Text](https://hackage.haskell.org/package/text/docs/Data-Text.html)
- [Haskell Documentation on Data.Text.Replace](https://hackage.haskell.org/package/text-icu-0.7.1/docs/Data-Text-Replace.html)
- [Tutorial on Pattern Matching in Haskell](https://www.youtube.com/watch?v=AFCJ47ieR00) (relevant for understanding how `replace` works)