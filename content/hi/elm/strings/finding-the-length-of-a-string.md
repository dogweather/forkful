---
title:                "स्ट्रिंग की लंबाई ज्ञात करना"
aliases: - /hi/elm/finding-the-length-of-a-string.md
date:                  2024-01-20T17:47:43.610335-07:00
model:                 gpt-4-1106-preview
simple_title:         "स्ट्रिंग की लंबाई ज्ञात करना"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elm/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों? (What & Why?)
स्ट्रिंग की लंबाई मापना मतलब यह देखना कि उसमें कितने कैरेक्टर हैं। प्रोग्रामर्स यह जानकारी इसलिए हासिल करते हैं ताकि वे डेटा वैलिडेशन, यूज़र इनपुट, या यूआई लेआउट को संभाल सकें।

## कैसे करें? (How to:)
```Elm
import Html exposing (text)
import String

-- स्ट्रिंग की लंबाई निकालने का फ़ंक्शन
stringLength : String -> Int
stringLength str =
  String.length str

-- मेन फ़ंक्शन जो वेब पेज पर आउटपुट दिखाएगा
main =
  text (String.fromInt (stringLength "नमस्ते!"))

-- आउटपुट: "7"
```
यहां "नमस्ते!" स्ट्रिंग की लंबाई 7 है क्योंकि इसमें सात कैरेक्टर्स हैं।

## गहराई से जानकारी (Deep Dive)
स्ट्रिंग की लंबाई निकालने का तरीका सालों से प्रोग्रामिंग में मौजूद है। Elm में, `String.length` फ़ंक्शन यह काम करता है। यह UTF-16 इनकोडिंग के आधार पर लंबाई देता है जो आमतौर पर यूनिकोड कैरेक्टर्स के लिए सही होता है। अगर आप यूनिकोड कोड पॉइंट्स के हिसाब से लंबाई चाहते हैं, तो आपको अलग फ़ंक्शन लिखना पड़ सकता है। 

स्ट्रिंग की लंबाई ढूंढने के लिए विकल्प भी हैं जैसे कि `String.foldl` जो स्ट्रिंग पर लूप लगा कर कैरेक्टर काउंट कर सकता है, लेकिन `String.length` ज्यादा सीधा और अधिक कुशल होता है।

## और भी जानकारी (See Also)
- Elm String डॉक्युमेंटेशन: [https://package.elm-lang.org/packages/elm/core/latest/String](https://package.elm-lang.org/packages/elm/core/latest/String)
