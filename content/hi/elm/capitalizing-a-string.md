---
title:                "एक स्ट्रिंग को कैपिटलाइज करना"
html_title:           "Elm: एक स्ट्रिंग को कैपिटलाइज करना"
simple_title:         "एक स्ट्रिंग को कैपिटलाइज करना"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elm/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

String को Capitalize करना का मतलब होता है उसके पहले अक्षर को बड़ा करना। यह इसलिए किया जाता है ताकि टेक्स्ट को पढ़ने में सुगमता महसूस की जा सके।

## कैसे करें:

Elm में, हम `toUpperCase` और `uncons` फंक्शंस का उपयोग करके किसी वाक्यांश को कैपिटलाइज कर सकते हैं:

```Elm
import String exposing (uncons, toUpper)

capitalize : String -> String
capitalize str =
   case uncons str of
      Nothing ->
         ""

      Just (firstChar, restOfString) ->
         String.toUpper (String.fromChar firstChar) ++ restOfString
```
यहाँ साउटपुट कैसा दिखाई देगा:
```Elm
capitalize "programming in Elm is fun"
--> "Programming in Elm is fun"
```

## गहराई से:

1. Elm में String capitalize करने की कई संभावनाएं हैं, लेकिन `toUpperCase` और `uncons` फंक्शंस का उपयोग करना सर्वश्रेष्ठ और सरल तरीका है। 

2. Elm प्राचीन फन्क्शनल प्रोग्रामिंग भाषाओं, जैसे कि ML और Haskell, के concepts पर आधारित है, लेकिन यह उनसे अधिक सुविधाजनक और उपयोगीरत है। 

3. यह implement करने में महत्वपूर्ण होता है की `uncons` फंक्शन कुछ भी हो सकता है - खाली string, एक single character, या और भी बड़े string। यह `Nothing` या `Just (firstChar, restOfString)` वापस करेगा जिससे `toUpperCase` फंक्शन को काम करने का आश्वासन होता है।

## और देखें:

- [Elm प्रोग्रामिंग गाइड](https://guide.elm-lang.org/)
- [String functions for Elm](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Elm के लिए ऑनलाइन प्लेग्राउंड](https://ellie-app.com/new)