---
date: 2024-01-20 17:42:22.115067-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) ."
lastmod: '2024-03-13T22:44:52.159052-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u092A\u0948\u091F\u0930\u094D\u0928 \u0938\u0947 \u092E\u0947\u0932 \u0916\
  \u093E\u0924\u0947 \u0905\u0915\u094D\u0937\u0930\u094B\u0902 \u0915\u094B \u0939\
  \u091F\u093E\u0928\u093E"
weight: 5
---

## How to: (कैसे करें:)
```Elm
module Main exposing (main)

import Html exposing (Html, text)
import Regex

removePattern : String -> String -> String
removePattern pattern input =
    Regex.replace Regex.All (Regex.regex pattern) (\_ -> "") input

main : Html msg
main =
    let
        originalString = "123 Elm Street - Elmwood City 456"
        cleanedString = removePattern "[\\d-]+" originalString
    in
    text cleanedString
```
सैंपल आउटपुट:
```
" Elm Street  Elmwood City "
```
यह एल्म कोड `[\\d-]+` पैटर्न से सभी अंकों और डैश को मिटा देगा।

## Deep Dive (गहराई से जानकारी):
पैटर्न के हिसाब से अक्षरों को मिटाना न केवल Elm में बल्कि कई अन्य प्रोग्रामिंग भाषा में आम है। यह क्रिया रेगुलर एक्सप्रेशन (Regex) का उपयोग करके की जाती है, जो एक शक्तिशाली पैटर्न मिलान उपकरण है। इतिहास में पहली बार केन थॉम्पसन ने QED टेक्स्ट एडिटर में Regex को लागू किया था। विकल्प के रूप में, कभी-कभी सीधी स्ट्रिंग मैनीपलेशन फंक्शन्स जैसे कि `String.filter`, `String.foldr` या `String.map` तकनीक का उपयोग कर सकते हैं। लेकिन, अगर जटिल पैटर्न की जरूरत हो तो Regex सबसे बेहतर है। Elm में, `Regex.replace` फंक्शन यह काम अत्यंत कुशलता से करता है।

## See Also (और देखें):
- Elm Regex package documentation: [https://package.elm-lang.org/packages/elm/regex/latest](https://package.elm-lang.org/packages/elm/regex/latest)
- Regular Expressions (Regex) Tutorial: [https://www.regular-expressions.info/](https://www.regular-expressions.info/)
- Elm String documentation for string manipulation alternatives: [https://package.elm-lang.org/packages/elm/core/latest/String](https://package.elm-lang.org/packages/elm/core/latest/String)
