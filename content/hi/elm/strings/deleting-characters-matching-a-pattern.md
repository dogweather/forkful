---
date: 2024-01-20 17:42:22.115067-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) \u092A\u0948\
  \u091F\u0930\u094D\u0928 \u0915\u0947 \u0939\u093F\u0938\u093E\u092C \u0938\u0947\
  \ \u0905\u0915\u094D\u0937\u0930\u094B\u0902 \u0915\u094B \u092E\u093F\u091F\u093E\
  \u0928\u093E \u0928 \u0915\u0947\u0935\u0932 Elm \u092E\u0947\u0902 \u092C\u0932\
  \u094D\u0915\u093F \u0915\u0908 \u0905\u0928\u094D\u092F \u092A\u094D\u0930\u094B\
  \u0917\u094D\u0930\u093E\u092E\u093F\u0902\u0917 \u092D\u093E\u0937\u093E \u092E\
  \u0947\u0902 \u0906\u092E \u0939\u0948\u0964 \u092F\u0939 \u0915\u094D\u0930\u093F\
  \u092F\u093E \u0930\u0947\u0917\u0941\u0932\u0930 \u090F\u0915\u094D\u0938\u092A\
  \u094D\u0930\u0947\u0936\u0928 (Regex) \u0915\u093E\u2026"
lastmod: '2024-04-05T22:51:06.848346-06:00'
model: gpt-4-1106-preview
summary: "(\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) \u092A\u0948\u091F\u0930\
  \u094D\u0928 \u0915\u0947 \u0939\u093F\u0938\u093E\u092C \u0938\u0947 \u0905\u0915\
  \u094D\u0937\u0930\u094B\u0902 \u0915\u094B \u092E\u093F\u091F\u093E\u0928\u093E\
  \ \u0928 \u0915\u0947\u0935\u0932 Elm \u092E\u0947\u0902 \u092C\u0932\u094D\u0915\
  \u093F \u0915\u0908 \u0905\u0928\u094D\u092F \u092A\u094D\u0930\u094B\u0917\u094D\
  \u0930\u093E\u092E\u093F\u0902\u0917 \u092D\u093E\u0937\u093E \u092E\u0947\u0902\
  \ \u0906\u092E \u0939\u0948\u0964 \u092F\u0939 \u0915\u094D\u0930\u093F\u092F\u093E\
  \ \u0930\u0947\u0917\u0941\u0932\u0930 \u090F\u0915\u094D\u0938\u092A\u094D\u0930\
  \u0947\u0936\u0928 (Regex) \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\
  \u0915\u0947 \u0915\u0940 \u091C\u093E\u0924\u0940 \u0939\u0948, \u091C\u094B \u090F\
  \u0915 \u0936\u0915\u094D\u0924\u093F\u0936\u093E\u0932\u0940 \u092A\u0948\u091F\
  \u0930\u094D\u0928 \u092E\u093F\u0932\u093E\u0928 \u0909\u092A\u0915\u0930\u0923\
  \ \u0939\u0948\u0964 \u0907\u0924\u093F\u0939\u093E\u0938 \u092E\u0947\u0902 \u092A\
  \u0939\u0932\u0940 \u092C\u093E\u0930 \u0915\u0947\u0928 \u0925\u0949\u092E\u094D\
  \u092A\u0938\u0928 \u0928\u0947 QED \u091F\u0947\u0915\u094D\u0938\u094D\u091F \u090F\
  \u0921\u093F\u091F\u0930 \u092E\u0947\u0902 Regex \u0915\u094B \u0932\u093E\u0917\
  \u0942 \u0915\u093F\u092F\u093E \u0925\u093E\u0964 \u0935\u093F\u0915\u0932\u094D\
  \u092A \u0915\u0947 \u0930\u0942\u092A \u092E\u0947\u0902, \u0915\u092D\u0940-\u0915\
  \u092D\u0940 \u0938\u0940\u0927\u0940 \u0938\u094D\u091F\u094D\u0930\u093F\u0902\
  \u0917 \u092E\u0948\u0928\u0940\u092A\u0932\u0947\u0936\u0928 \u092B\u0902\u0915\
  \u094D\u0936\u0928\u094D\u0938 \u091C\u0948\u0938\u0947 \u0915\u093F `String.filter`,\
  \ `String.foldr` \u092F\u093E `String.map` \u0924\u0915\u0928\u0940\u0915 \u0915\
  \u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0930 \u0938\u0915\u0924\u0947 \u0939\
  \u0948\u0902\u0964 \u0932\u0947\u0915\u093F\u0928, \u0905\u0917\u0930 \u091C\u091F\
  \u093F\u0932 \u092A\u0948\u091F\u0930\u094D\u0928 \u0915\u0940 \u091C\u0930\u0942\
  \u0930\u0924 \u0939\u094B \u0924\u094B Regex \u0938\u092C\u0938\u0947 \u092C\u0947\
  \u0939\u0924\u0930 \u0939\u0948\u0964 Elm \u092E\u0947\u0902, `Regex.replace` \u092B\
  \u0902\u0915\u094D\u0936\u0928 \u092F\u0939 \u0915\u093E\u092E \u0905\u0924\u094D\
  \u092F\u0902\u0924 \u0915\u0941\u0936\u0932\u0924\u093E \u0938\u0947 \u0915\u0930\
  \u0924\u093E \u0939\u0948\u0964."
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
