---
date: 2024-01-20 17:51:20.621175-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) Haskell\
  \ \u092E\u0947\u0902 string interpolation \u0915\u0930\u0928\u0947 \u0915\u0947\
  \ \u0932\u093F\u090F \u0939\u092E `printf` \u092F\u093E `interpolate` library \u0915\
  \u093E use \u0915\u0930 \u0938\u0915\u0924\u0947 \u0939\u0948\u0902. \u0928\u0940\
  \u091A\u0947 \u0926\u094B examples \u0939\u0948\u0902."
lastmod: '2024-03-13T22:44:52.380282-06:00'
model: gpt-4-1106-preview
summary: "Haskell \u092E\u0947\u0902 string interpolation \u0915\u0930\u0928\u0947\
  \ \u0915\u0947 \u0932\u093F\u090F \u0939\u092E `printf` \u092F\u093E `interpolate`\
  \ library \u0915\u093E use \u0915\u0930 \u0938\u0915\u0924\u0947 \u0939\u0948\u0902\
  ."
title: "\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0907\u0902\u091F\u0930\u092A\
  \u094B\u0932\u0947\u0936\u0928"
weight: 8
---

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
