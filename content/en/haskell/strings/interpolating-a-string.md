---
title:                "Interpolating a string"
aliases: - /en/haskell/interpolating-a-string.md
date:                  2024-01-20T17:50:47.284316-07:00
model:                 gpt-4-1106-preview
simple_title:         "Interpolating a string"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
String interpolation lets you embed variables directly in strings. It's done for convenience and readability—no plus signs or function calls needed to construct your message.

## How to:

In Haskell, string interpolation isn't baked in, but with the `interpolate` package, you can get pretty close. First, ensure you have the package:

```bash
cabal update
cabal install interpolate
```

Now, write some Haskell:

```haskell
{-# LANGUAGE QuasiQuotes #-}
import Data.String.Interpolate (i)

main :: IO ()
main = do
    let name = "world"
    let greeting = [i|Hello, #{name}!|]
    putStrLn greeting
```

Run it:

```
Hello, world!
```

## Deep Dive

Historically, Haskell didn't come with string interpolation out of the box. It's a feature more common in scripting languages. Interpolation in Haskell became smoother with the development of quasiquoters, which allow you to define your own custom syntax—like our `i` for interpolating strings.

Alternatives? Sure, use `printf` from `Text.Printf`, or concatenate strings and variables with `++`. But these lack the elegance and simplicity of interpolation.

Implementation-wise, `interpolate` transforms your interpolated strings into regular Haskell strings at compile-time using Template Haskell, so there's no performance hit when running your code. It's clever and clean, just like Haskell.

## See Also

- [Hackage - interpolate package](https://hackage.haskell.org/package/interpolate)
- [Hackage - Text.Printf module](https://hackage.haskell.org/package/base/docs/Text-Printf.html)
- [Haskell Wiki - Quasiquotation](https://wiki.haskell.org/Quasiquotation)
- For rich templating, check out [Hackage - Mustache templates](https://hackage.haskell.org/package/mustache)
