---
date: 2024-01-20 17:47:43.610335-07:00
description: "\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0915\u0940 \u0932\
  \u0902\u092C\u093E\u0908 \u092E\u093E\u092A\u0928\u093E \u092E\u0924\u0932\u092C\
  \ \u092F\u0939 \u0926\u0947\u0916\u0928\u093E \u0915\u093F \u0909\u0938\u092E\u0947\
  \u0902 \u0915\u093F\u0924\u0928\u0947 \u0915\u0948\u0930\u0947\u0915\u094D\u091F\
  \u0930 \u0939\u0948\u0902\u0964 \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\
  \u092E\u0930\u094D\u0938 \u092F\u0939 \u091C\u093E\u0928\u0915\u093E\u0930\u0940\
  \ \u0907\u0938\u0932\u093F\u090F \u0939\u093E\u0938\u093F\u0932 \u0915\u0930\u0924\
  \u0947 \u0939\u0948\u0902 \u0924\u093E\u0915\u093F \u0935\u0947 \u0921\u0947\u091F\
  \u093E \u0935\u0948\u0932\u093F\u0921\u0947\u0936\u0928, \u092F\u0942\u091C\u093C\
  \u0930 \u0907\u0928\u092A\u0941\u091F, \u092F\u093E\u2026"
lastmod: '2024-03-13T22:44:52.170931-06:00'
model: gpt-4-1106-preview
summary: "\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0915\u0940 \u0932\u0902\
  \u092C\u093E\u0908 \u092E\u093E\u092A\u0928\u093E \u092E\u0924\u0932\u092C \u092F\
  \u0939 \u0926\u0947\u0916\u0928\u093E \u0915\u093F \u0909\u0938\u092E\u0947\u0902\
  \ \u0915\u093F\u0924\u0928\u0947 \u0915\u0948\u0930\u0947\u0915\u094D\u091F\u0930\
  \ \u0939\u0948\u0902\u0964 \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\
  \u0930\u094D\u0938 \u092F\u0939 \u091C\u093E\u0928\u0915\u093E\u0930\u0940 \u0907\
  \u0938\u0932\u093F\u090F \u0939\u093E\u0938\u093F\u0932 \u0915\u0930\u0924\u0947\
  \ \u0939\u0948\u0902 \u0924\u093E\u0915\u093F \u0935\u0947 \u0921\u0947\u091F\u093E\
  \ \u0935\u0948\u0932\u093F\u0921\u0947\u0936\u0928, \u092F\u0942\u091C\u093C\u0930\
  \ \u0907\u0928\u092A\u0941\u091F, \u092F\u093E\u2026"
title: "\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0915\u0940 \u0932\u0902\
  \u092C\u093E\u0908 \u091C\u094D\u091E\u093E\u0924 \u0915\u0930\u0928\u093E"
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
