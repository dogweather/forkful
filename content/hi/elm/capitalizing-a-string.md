---
title:                "स्ट्रिंग को कैपिटलाइज़ करना"
html_title:           "C: स्ट्रिंग को कैपिटलाइज़ करना"
simple_title:         "स्ट्रिंग को कैपिटलाइज़ करना"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elm/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
स्ट्रिंग को कैपिटलाइज़ करना मतलब होता है कि एक टेक्स्ट में प्रत्येक शब्द का पहला अक्षर बड़ा (या अपरकेस) होना। प्रोग्रामर अक्सर विशिष्ट शब्दों को हाइलाइट करने, हेडिंग्स या टाइटल्स को फॉर्मेट करने के लिए स्ट्रिंग्स को कैपिटलाइज़ करते हैं।

## How to: (कैसे करें:)
Elm में String module एक built-in function `toUpper` प्रदान करता है जिसे कैपिटलाइज़ करने के लिए इस्तेमाल किया जा सकता है।

```Elm
import String

capitalizeString : String -> String
capitalizeString str =
    String.toUpper str

main =
    String.words "नमस्ते दुनिया!" |> List.map capitalizeString |> String.join " "
```

उपरोक्त एक्ज़ाम्पल में, `capitalizeString` फंक्शन सभी इनपुट स्ट्रिंग्स को अपरकेस में बदल देता है। सैंपल आउटपुट होगा:

```
"नमस्ते दुनिया!"
```

## Deep Dive (गहराई से जानकारी)
स्ट्रिंग कैपिटलाइजेशन पहले महत्वपूर्ण था जब कम्प्यूटर्स के इंटरफेस पत्रिकाओं और टाइपराइटर्स से प्रेरित थे। आज भी यह उपयोगी है जैसे कि प्रोटोकॉल और एपीआई में के-वैल्यू पेयर्स की कुंजियों में। Elm में, `String.toUpper` को पूरे स्ट्रिंग कैपिटलाइजेशन के लिए इस्तेमाल किया जाता है, और अब तक इबिल्ट `capitalize` फंक्शन मौजूद नहीं है।

जैसा कि Elm एक purely functional language है, आपको अपने `capitalize` फंक्शन को लिखने के लिए इम्यूटेबल डेटा स्ट्रक्चर्स और फंक्शन्स का इस्तेमाल करना चाहिए। यह कीपॉइंट ईकोसिस्टम में स्थिरता और प्रिडिक्टेबिलिटी लाते हैं।

## See Also (देखें भी)
- Elm `String` module documentation: [https://package.elm-lang.org/packages/elm/core/latest/String](https://package.elm-lang.org/packages/elm/core/latest/String)
- Elm style guide for string manipulation: [https://elm-lang.org/docs/style-guide](https://elm-lang.org/docs/style-guide)
- Elm-community/string-extra package for additional string functions: [https://package.elm-lang.org/packages/elm-community/string-extra/latest/](https://package.elm-lang.org/packages/elm-community/string-extra/latest/)
