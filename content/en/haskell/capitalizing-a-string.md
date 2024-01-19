---
title:                "Capitalizing a string"
html_title:           "Haskell recipe: Capitalizing a string"
simple_title:         "Capitalizing a string"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Capitalizing Strings in Haskell

## What & Why?
Capitalizing a string simply means converting its initial character to uppercase while leaving the rest unchanged. Programmers often capitalize strings for text normalization, enhancing readability, or satisfying specific data requirements. 

## How to:
In Haskell, the `toUpper` function from the `Data.Char` module capitalizes a letter. To capitalize a string, apply this function to the first character.

```Haskell
import Data.Char

capitalize :: String -> String
capitalize "" = ""
capitalize (head:tail) = toUpper head : tail

main = print (capitalize "haskell is cool!")
```
When you run the `main` function, output would be: `Haskell is cool!`

## Deep Dive
Capitalization methods may vary across languages, but the basic principle remains constant: transforming the first character to uppercase. Relatively young in the programming language landscape, Haskell takes a more functional approach.

Haskell's `toUpper` function originates from the Unicode consortium's standard casing rules, which is why it works well for letters beyond ASCII.

Alternatives to the `toUpper` method include direct unicode manipulation or custom functions, but such solutions are often more complicated and unnecessary. 

Haskell capitalizes a string lazily, meaning it only operates on the string when it's needed. This feeds into the language's efficiency, ensuring that not a single computation cycle goes to waste.

## See Also
* `toUpper` function from `Data.Char` module: http://hackage.haskell.org/package/base-4.15.0.0/docs/Data-Char.html#v:toUpper
* Haskell official documentation: https://www.haskell.org/documentation/
* Text normalization in NLP: https://nlp.stanford.edu/IR-book/html/htmledition/normalization-equivalence-classing-of-terms-1.html