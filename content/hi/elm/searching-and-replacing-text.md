---
title:                "पाठ की खोज और प्रतिस्थापन"
html_title:           "Bash: पाठ की खोज और प्रतिस्थापन"
simple_title:         "पाठ की खोज और प्रतिस्थापन"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elm/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
टेक्स्ट को खोजना और बदलना मतलब है कि हम एक टेक्स्ट इनपुट को पर्स करके स्पेसिफिक पैटर्न की खोज करते हैं, और उसे नई वैल्यू के साथ बदलते हैं। यह यादा काम में आता है जब हमारे पास बड़े डेटा से काम करना होता है, जैसे कि फ़ाइलें, वेब पेज, या डेटाबेस के रिकॉर्ड।

## कैसे करें:
न्यायाधिकारी (Regular Expression) हमें इसमें मदद करता है। यहां एक basic उदाहरण है:

```Elm
module Main exposing (..)
import Regex
text = "नमस्ते Elm, आपका स्वागत है Elm!"
re = Regex.fromString "Elm" |> Result.withDefault Regex.never
newText = Regex.replace re (always "Hindi") text
```

इस उदाहरण में, हमने `"Elm"` खोजकर उसे `"Hindi"` के साथ बदल देने का प्रयास किया। Output होगा: `"नमस्ते Hindi, आपका स्वागत है Hindi!"`

## गहराई की ओर:
Elm एक वेबफ्रंट एंड लैंग्वेज है जिसने 2012 में एवन चापलीक्य द्वारा डेवलप होना शुरू किया था। Java, Python में भी ऐसे ही फंक्शन्स उपलब्ध हैं लेकिन Elm इसे प्रभावी तरीके से ट्रीट करता है। Elm यह ऑपरेशन परफॉर्म करने के लिए Regex पैकेज का उपयोग करता है जो कि एक छोटी और संक्षिप्त library है।

## अधिक जानकारी के लिए:
- [Elm का tutorial](https://elm-lang.org/docs)
- [Elm का official guide](https://guide.elm-lang.org/)
- [Regular Expressions in Elm](https://package.elm-lang.org/packages/elm/regex/latest/)