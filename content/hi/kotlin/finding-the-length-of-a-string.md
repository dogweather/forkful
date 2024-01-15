---
title:                "स्ट्रिंग की लंबाई का पता लगाना"
html_title:           "Kotlin: स्ट्रिंग की लंबाई का पता लगाना"
simple_title:         "स्ट्रिंग की लंबाई का पता लगाना"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## लंबाई को पता करने की क्यों

पाठ की लंबाई को पता करने या जानने की आवश्यकता उन लोगों को होती है जो अपने कोड में स्ट्रिंग को समायोजित करने की कोशिश कर रहे होते हैं। इससे वे स्ट्रिंग को सही ढंग से माप सकते हैं और उसे उन्हें अपने कोड के अनुरूप बदल सकते हैं।

## कैसे करें

```Kotlin
// एक साधारण स्ट्रिंग बनाएं
val myString = "हेलो वर्ल्ड"

// स्ट्रिंग की लंबाई का पता करें 
val length = myString.length

// परिणाम प्रिंट करें
print("मेरे पाठ की लंबाई है: $length")
```
```
आउटपुट: मेरे पाठ की लंबाई है: 10
```

## गहराई में जाएं

स्ट्रिंग की लंबाई की क्षमता `length` वस्तु में निहित होती है। यह फ़ंक्शन किसी भी स्ट्रिंग पर लागू किया जा सकता है और उस स्ट्रिंग को हमेशा सही ढंग से मापता है। इसके अलावा, हम इस फ़ंक्शन के द्वारा स्ट्रिंग के अंतराल का मापना भी कर सकते हैं।

## यहां देखें

- [Kotlin स्ट्रिंग में लंबाई कैसे पता करें](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/length.html)
- [Kotlin - पाठ की लंबाई को पूर्णांक में कैसे प्राप्त करें](https://www.tutorialkart.com/kotlin/strings/get-length-of-string-in-kotlin-with-java-methods/)
- [कोटलिन स्ट्रिंग के साथ विभिन्न ऑपरेशन कैसे करें](https://www.geeksforgeeks.org/kotlin-strings/)
## देखें भी

- [कोटलिन का उपयोग करके स्ट्रिंग बनाएं और संपादन करें](https://techvidvan.com/tutorials/kotlin-strings/)