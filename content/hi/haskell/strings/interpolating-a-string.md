---
title:                "स्ट्रिंग इंटरपोलेशन"
aliases:
- /hi/haskell/interpolating-a-string.md
date:                  2024-01-20T17:51:20.621175-07:00
model:                 gpt-4-1106-preview
simple_title:         "स्ट्रिंग इंटरपोलेशन"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
String interpolation मतलब है एक string के अंदर variable की values को insert करना. Programmers इसे use करते हैं क्योंकि इससे dynamic strings बनाना आसान हो जाता है और अपने code को अधिक readable और maintainable बना सकते हैं.

## How to: (कैसे करें:)
Haskell में string interpolation करने के लिए हम `printf` या `interpolate` library का use कर सकते हैं. नीचे दो examples हैं:

```Haskell
-- Using printf from Text.Printf
import Text.Printf (printf)

main :: IO ()
main = do
  let name = "विशाल"
  let year = 2023
  printf "नमस्ते, मेरा नाम %s है और इस साल है %d.\n" name year

-- Expected output:
-- नमस्ते, मेरा नाम विशाल है और इस साल है 2023.
```

```Haskell
-- Using interpolation library
{-# LANGUAGE QuasiQuotes #-}
import Data.String.Interpolate (i)

main :: IO ()
main = do
  let name = "सीमा"
  let age = 25
  putStrLn [i|नमस्ते, मेरा नाम #{name} है और मेरी उम्र #{age} साल है.|]

-- Expected output:
-- नमस्ते, मेरा नाम सीमा है और मेरी उम्र 25 साल है.
```

## Deep Dive (गहराई से जानकारी)
string interpolation एक मजबूत feature है जो बहुत सारे programming languages में मिलता है, Haskell में यह quasi-quotes के through possible है. `printf` function C language से inspired है, जो typed sprintf की Haskell version है. `interpolate` library Haskell प्रोग्रामर्स के लिए अधिक modern और convenient solution प्रदान करती है जिसमें quasi-quotes का use किया जाता है. Haskell में interpolation करना static type checking से compromise किए बिना होता है, जो safety को ensure करता है.

## See Also (और जानकारी के लिए)
- [Haskell `printf` documentation](https://hackage.haskell.org/package/base-4.16.1.0/docs/Text-Printf.html)
- [Data.String.Interpolate library on Hackage](https://hackage.haskell.org/package/interpolate)
- [Haskell Wiki on Quasiquotation](https://wiki.haskell.org/Quasiquotation)
