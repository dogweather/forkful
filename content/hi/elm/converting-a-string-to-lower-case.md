---
title:                "एक स्ट्रिंग को लोअर केस में परिवर्तित करना"
html_title:           "Kotlin: एक स्ट्रिंग को लोअर केस में परिवर्तित करना"
simple_title:         "एक स्ट्रिंग को लोअर केस में परिवर्तित करना"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elm/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
मूल रूप से, स्ट्रिंग को लोअरकेस में बदलने का मतलब होता है उस स्ट्रिंग के सभी अक्षरों को छोटे अक्षरों में बदलना। प्रोग्रामर इसे आमतौर पर तब करते हैं जब वे इनपुट के सेंसिटिविटी को कम करना चाहते हैं, ताकि "एप्पल" और "Apple" को समान माना जा सके।

## कैसे करें:
एल्म में, हम `toLower` फ़ंक्शन का उपयोग कर सकते हैं, जो `String` मॉड्यूल में पाया जाता है। यहाँ कुछ उदाहरण हैं:

```Elm 
import String

lowercaseString : String -> String
lowercaseString str =
    String.toLower str

main =
    print (lowercaseString "HELLO, वर्ल्ड!")
```

यह कोड "hello, वर्ल्ड!" निर्गम देगा।

## गहराई में:
`toLower` फंक्शन का निर्माण Elm की `String` मॉड्यूल के साथ किया गया था, जो परत दर परत भाषा की पहचान और मुद्रण की कठिनाईयों से निपटने में हमें मदद करता है। इसके विकल्प के रूप में, आप अपने स्ट्रिंग मानिपुलेशन कार्यों को स्वयं लिख सकते हैं, लेकिन यह समय-खपत और त्रुटि से भरा हो सकता है। इसे बैकग्राउंड में, Elm `toLower` फंक्शन Unicode स्ट्रिंग को सही ढंग से संबोधित करता है, जिससे हमें एन्कोडिंग और डेकोडिंग के बारे में चिंता करने की जरूरत नहीं होती।

## और देखें:
- Elm String toLower Documentation: <https://package.elm-lang.org/packages/elm/core/latest/String#toLower>.
- String Manipulation in Elm: <https://elmprogramming.com/string.html>.
- Unicode and You: <https://www.joelonsoftware.com/2003/10/08/the-absolute-minimum-every-software-developer-absolutely-positively-must-know-about-unicode-and-character-sets-no-excuses/>.
- "Character Encoding for Beginners": <http://www.kunststube.net/encoding/>.