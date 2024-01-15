---
title:                "स्ट्रिंग को लोअर केस में बदलना"
html_title:           "Clojure: स्ट्रिंग को लोअर केस में बदलना"
simple_title:         "स्ट्रिंग को लोअर केस में बदलना"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## क्यों

कोई भी प्रोग्रामिंग भाषा में, स्ट्रिंग्स (strings) अक्सर सबसे अधिक प्रयोग होने वाले डेटा टाइप्स होते हैं। मूल शब्दों को प्रोसेस करने के लिए, स्ट्रिंग्स को लोअर केस में ट्रांसफॉर्म करना आसान और उपयोगी हो सकता है। 

## कैसे करें

```Clojure
(.toLowerCase "HELLO WORLD") 
; "hello world"
```

स्ट्रिंग्स को लोअर केस में कनवर्ट (convert) करने के लिए, आपको `toLowerCase` फ़ंक्शन का उपयोग करना होगा। यह फ़ंक्शन एक स्ट्रिंग को लोअर केस में रिटर्न (return) करता है। आप इसे अपने स्ट्रिंग के साथ टाइप करके देख सकते हैं कि वह आपकी प्रक्रिया के लिए सही रिज़ल्ट देता है।

## गहन जानकारी

लोअर केस (lower case) एक स्ट्रिंग के शब्दों को छोटा (small) और सबसे उपयुक्त (appropriate) रूप में प्रदर्शित करता है। इसका उपयोग डेटा को साफ़ और संरचित रखने के लिए किया जाता है - जैसे कि सर्च इंजन्स (search engines) की प्रतिक्रिया को देखें। स्ट्रिंग्स को लोअर केस में ट्रांसफॉर्म करने के लिए डिफॉल्ट फ़ंक्शन `toLowerCase` जैसे बहुत से उपकरण और लाइब्रेरी (libraries) उपलब्ध हैं। 

## वैसा देखें

[Official Clojure Docs on Strings](https://clojure.org/reference/data_structures#Strings)

[Clojure Cookbook on Converting Strings to Lower Case](https://clojure-cookbook.com/strings/conversions.html#lower-case-strings)

[Clojure String Library - string-utils](https://github.com/clojurewerkz/string-utils)