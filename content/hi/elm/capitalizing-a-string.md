---
title:                "स्ट्रिंग को कैपिटलाइज़ करना"
html_title:           "Elm: स्ट्रिंग को कैपिटलाइज़ करना"
simple_title:         "स्ट्रिंग को कैपिटलाइज़ करना"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elm/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# अगला क्या अहै?

Capitalizing a string एक आम प्रोग्रामिंग टेक्निक है जिसमें एक स्ट्रिंग के पहले अक्षर को बड़ा किया जाता है। यह क्रिया अक्सर स्ट्रिंग को प्रिंट करने से पहले किया जाता है ताकि उसकी प्रतिष्ठा बढ़े और उसे पढ़ने में आसानी हो। प्रोग्रामर्स ऐसा करने के लिए चुनौतियों का सामना करते हैं और इससे उनके कोड को अधिक प्रोफेशनल दिखाने में मदद मिलती है। 

# कैसे करें?

```Elm
import String exposing (toUpper)

string = "hello"
capitalized = toUpper string
```

इस कोड में, हमने `String` लाइब्रेरी से `toUpper` फ़ंक्शन को इम्पोर्ट किया है जो कि एक स्ट्रिंग को बड़ा करने के लिए उपयोग किया जाता है। फिर हमने अपनी स्ट्रिंग को `string` नामक एक वेरिएबल में स्टोर किया और `toUpper` फ़ंक्शन का उपयोग करके उसे `capitalized` नामक एक और वेरिएबल में स्टोर किया। जब हम अपनी स्ट्रिंग को प्रिंट करते हैं, तो इस तरह का आउटपुट मिलता है: `"HELLO"` 

# गहराई से जानिए

1. इस टेक्निक का प्रयोग पहले एस्सम्ब्ली और असेंबलर के डेवलपर्स द्वारा किया जाता था जहां वे रेजिस्टर्स को अपडेट करने के लिए उपयोग करते थे।

2. इसके अलावा, आप `toUpper` के बजाय `toLower` फ़ंक्शन का भी उपयोग कर सकते हैं जो एक स्ट्रिंग के पहले अक्षर को छोटा करता है।

3. इस टेक्निक के माध्यम से आप अपने स्ट्रिंग के केस भी बदल सकते हैं। आप `toTitle` फ़ंक्शन का भी उपयोग कर सकते हैं जो एक स्ट्रिंग के प्रथम अक्षर को अपने शब्दों के शीर्षक के रूप में बदलता है।

# और भी देखें

- [Official Elm Documentation for Strings](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Blog post on String manipulation in Elm](https://ohanhi.github.io/base/string-manipulation-in-elm/)
- [Elm REPL to try out string functions](https://ellie-app.com/new)