---
title:                "पाठ खोजना और बदलना"
aliases:
- /hi/elm/searching-and-replacing-text.md
date:                  2024-01-20T17:57:59.034720-07:00
model:                 gpt-4-1106-preview
simple_title:         "पाठ खोजना और बदलना"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elm/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why? - क्या और क्यों?

किसी टेक्स्ट में विशेष शब्द खोजना और उसे बदलना एक अक्सरीयत वाली क्रिया है जिसे हम प्रोग्रामिंग में भी करते हैं। प्रोग्रामर्स इसे डेटा को अपडेट करने, गलतियों को सही करने, या डेटा को नए फॉर्मेट में बदलने के लिए करते हैं।

## How to: - कैसे करें:

```Elm
import Html exposing (text)
import String

main =
  let
    originalText = "गलत टेक्स्ट गलत टेक्स्ट"
    newText = String.replace "गलत" "सही" originalText
  in
    text newText
```

इस कोड को चलाने पर आपको संकल्पनात्मक आउटपुट मिलेगा: "सही टेक्स्ट सही टेक्स्ट"

## Deep Dive - गहराई से जानकारी:

टेक्स्ट खोजना और बदलना हमेशा से एक महत्वपूर्ण क्रिया रही है, फिर चाहे वह पुराने समय की बैच स्क्रिप्ट में हो या आधुनिक वेब ऐप्लिकेशन्स में। Elm, जो कि एक फंक्शनल प्रोग्रामिंग भाषा है, में `String.replace` फंक्शन का इस्तेमाल करके यह कार्य आसानी से किया जा सकता है। यह फंक्शन तीन पैरामीटर्स लेता है: खोजने वाला शब्द, बदलने वाला शब्द, और मूल टेक्स्ट।

विकल्पों के रूप में, अन्य प्रोग्रामिंग भाषाओं में भी इसी तरीके के फंक्शन होते हैं, जैसे कि JavaScript में `.replace()`, Python में `.replace()` आदि। हलांकि, Elm इसे प्योर फंक्शन द्वारा करता है जिसमें आप सीधे आर्ग्युमेंट्स में परिवर्तन की जानकारी देकर क्रिया को पूरा कर सकते हैं।

## See Also - देखें अन्य:

- Elm भाषा के आधिकारिक `String` मॉड्यूल डॉक्युमेंटेशन: [Elm String Module](http://package.elm-lang.org/packages/elm/core/latest/String#replace)
- टेक्स्ट प्रोसेसिंग के लिए `String` फंक्शन्स की एक श्रृंखला: [Elm String Functions](http://package.elm-lang.org/packages/elm/core/latest/String)
- Elm के साथ अधिक कॉम्प्लेक्स पैटर्न्स के लिए `Regex` पैकेज: [Elm Regex Package](http://package.elm-lang.org/packages/elm/regex/latest)
