---
title:                "पैटर्न से मेल खाते अक्षरों को हटाना"
date:                  2024-01-20T17:42:22.115067-07:00
model:                 gpt-4-1106-preview
simple_title:         "पैटर्न से मेल खाते अक्षरों को हटाना"

category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elm/deleting-characters-matching-a-pattern.md"
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
