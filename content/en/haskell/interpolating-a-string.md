---
title:                "Interpolating a string"
html_title:           "Arduino recipe: Interpolating a string"
simple_title:         "Interpolating a string"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

String interpolation is about substitifying variables in a predefined text (string). Why bother? It simplifies creation of dynamic strings and makes your code easier to write, read, and maintain.

## How to:

Haskell doesn't include built-in string interpolation, but we've got a handy library called `Text.Printf` for similar jobs. Here's a simple example:

```Haskell
import Text.Printf (printf)

main = printf "Hello, %s! You are number %d. \n" ("Haskell"::String) (1::Int)
```
Running this code will output: 
```
Hello, Haskell! You are number 1.
```
Variables "Haskell" and 1 are inserted in place of `%s` and `%d` respectively. 

## Deep Dive

Historically, Haskell aimed at being a pure functional language, avoiding built-in side effects like string interpolation found in many scripting languages. Rather than interpolation, it leverages advanced type system and pure functions, using libraries like `Text.Printf` and quasi-quoting libraries like `neat-interpolation`.

Alternatives to `Text.Printf` include the `formatting` library for more complex cases, and `neat-interpolation` for multiline string interpolation. However, remember that each additional library increases dependency and may affect code portability.

Implementation details in `Text.Printf` are interesting. It leverages Haskell's type system and polymorphism to provide flexible formatting. It's not true string interpolation, rather a type-safe version of `printf` found in C/C++.

## See Also

1. [printf in Haskell?](https://stackoverflow.com/questions/42798277/printf-in-haskell)
2. [Compilation of neat-interpolation](https://hackage.haskell.org/package/neat-interpolation)
3. [Text.Printf library doc](http://hackage.haskell.org/package/base-4.14.1.0/docs/Text-Printf.html)
4. [formatting library](https://hackage.haskell.org/package/formatting)