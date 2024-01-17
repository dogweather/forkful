---
title:                "नियमित अभिव्यक्तियों का उपयोग करना"
html_title:           "Elm: नियमित अभिव्यक्तियों का उपयोग करना"
simple_title:         "नियमित अभिव्यक्तियों का उपयोग करना"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elm/using-regular-expressions.md"
---

{{< edit_this_page >}}

## क्या एल्म में नियमित अभिव्यक्तियों का उपयोग क्या है?

नियामक अभिव्यक्तियों का उपयोग करके, आप अपनी डेटा में खोज और फिल्टर कर सकते हैं और इसे व्‍याख्‍या करने में आसानी होती है। कई बार डेटा संपादित करते समय, हमें एक निश्चित गतिविधि को खोजने की आवश्यकता होती है और उसे शामिल करने के लिए नियमित अभिव्यक्तियों का उपयोग किया जा सकता है।

## कैसे करें:

```Elm
import Regex exposing (..)

phoneRegex : Regex
phoneRegex =
    regex "([0-9]{3})-([0-9]{3})-([0-9]{4})" -- नंबर को स्थानांतरित करें

result : Maybe ( String, String, String )
result =
    firstMatch phoneRegex "123-456-7890" -- अनुकूलित करें

```

यहां, हमने `Regex` लाइब्ररी से `import` किया है और नियमित संचालित करने के लिए `phoneRegex` इंस्‍टेंस बनाया है। फिर, `firstMatch` की मदद से हम अपने फ़ोन नंबर स्ट्रिंग को नियमित से मिलाते हैं और रिजल्ट को `String` लाइब्ररी का उपयोग करके प्रिंट करते है।

## गहराई में जाएं:

नियमित अभिव्यक्तियों का उपयोग प्रोग्रामिंग के साथ एक मजबूत इतिहास रखता है। इनका प्रारंभ वाे 1950 के दशक में किया गया था और वे उस समय से हमारे पूरे कंप्यूटिंग इतिहास में एक महत्वपूर्ण भूमिका निभाते आए हैं। अन्य विकल्पों में जैसे की string parsing और string manipulation में regex का उपयोग किया गया क्योंकि यह बहुत ही शक्तिशाली और अन्य मेथडों से तेज है। एल्म में नियमित अभिव्यक्तियों के अन्य उपयोग में जिसमें ये शामिल होते हैं, form validation, text search और string replacement शामिल हैं।

## देखें भी:

- [Elm Introduction](https://guide.elm-lang.org/)
- [Intro to Regex](https://medium.com/@BenjaminGruenbaum/intro-to-regexes-in-elm-a6679f3fdd7a)
- [Elm Regex Library Documentation](https://package.elm-lang.org/packages/elm/regex/latest/)