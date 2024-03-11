---
date: 2024-01-20 17:42:22.115067-07:00
description: "\u0905\u0915\u094D\u0937\u0930\u094B\u0902 \u0915\u094B \u092A\u0948\
  \u091F\u0930\u094D\u0928 \u0915\u0947 \u0939\u093F\u0938\u093E\u092C \u0938\u0947\
  \ \u092E\u093F\u091F\u093E\u0928\u093E \u092E\u0924\u0932\u092C \u0939\u0948 \u090F\
  \u0915 \u0928\u093F\u0936\u094D\u091A\u093F\u0924 \u0936\u094D\u0930\u0943\u0902\
  \u0916\u0932\u093E \u0938\u0947 \u091A\u0941\u0928\u093F\u0902\u0926\u093E \u0905\
  \u0915\u094D\u0937\u0930\u094B\u0902 \u0915\u094B \u0939\u091F\u093E\u0928\u093E\
  \u0964 \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930\u094D\u0938\
  \ \u092F\u0939 \u0915\u093E\u092E \u0915\u0930\u0924\u0947 \u0939\u0948\u0902 \u0921\
  \u093E\u091F\u093E \u0915\u094B \u0938\u093E\u092B \u0915\u0930\u0928\u0947 \u0914\
  \u0930 \u092E\u093E\u0928\u094D\u092F\u0924\u093E \u0915\u0940\u2026"
lastmod: '2024-03-11T00:14:26.058137-06:00'
model: gpt-4-1106-preview
summary: "\u0905\u0915\u094D\u0937\u0930\u094B\u0902 \u0915\u094B \u092A\u0948\u091F\
  \u0930\u094D\u0928 \u0915\u0947 \u0939\u093F\u0938\u093E\u092C \u0938\u0947 \u092E\
  \u093F\u091F\u093E\u0928\u093E \u092E\u0924\u0932\u092C \u0939\u0948 \u090F\u0915\
  \ \u0928\u093F\u0936\u094D\u091A\u093F\u0924 \u0936\u094D\u0930\u0943\u0902\u0916\
  \u0932\u093E \u0938\u0947 \u091A\u0941\u0928\u093F\u0902\u0926\u093E \u0905\u0915\
  \u094D\u0937\u0930\u094B\u0902 \u0915\u094B \u0939\u091F\u093E\u0928\u093E\u0964\
  \ \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930\u094D\u0938 \u092F\
  \u0939 \u0915\u093E\u092E \u0915\u0930\u0924\u0947 \u0939\u0948\u0902 \u0921\u093E\
  \u091F\u093E \u0915\u094B \u0938\u093E\u092B \u0915\u0930\u0928\u0947 \u0914\u0930\
  \ \u092E\u093E\u0928\u094D\u092F\u0924\u093E \u0915\u0940\u2026"
title: "\u092A\u0948\u091F\u0930\u094D\u0928 \u0938\u0947 \u092E\u0947\u0932 \u0916\
  \u093E\u0924\u0947 \u0905\u0915\u094D\u0937\u0930\u094B\u0902 \u0915\u094B \u0939\
  \u091F\u093E\u0928\u093E"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
अक्षरों को पैटर्न के हिसाब से मिटाना मतलब है एक निश्चित श्रृंखला से चुनिंदा अक्षरों को हटाना। प्रोग्रामर्स यह काम करते हैं डाटा को साफ करने और मान्यता की प्रक्रिया में।

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
