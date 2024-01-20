---
title:                "एक स्ट्रिंग को लोअर केस में परिवर्तित करना"
html_title:           "Kotlin: एक स्ट्रिंग को लोअर केस में परिवर्तित करना"
simple_title:         "एक स्ट्रिंग को लोअर केस में परिवर्तित करना"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## क्या और क्यों? 
एक String को Lower Case में बदलना ऐसा नियंत्रण है जिसमें सभी अक्षरों को उनके छोटे (लोअरकेस) संस्करण में बदला जाता है। यह इसलिए किया जाता है ताकि हम पाठ की तुलना कर सकें, उद्घाटन कर सकें एवं अन्य सामान्य कार्रवाई रूप में उपयोग कर सकें, बिना बड़े-छोटे अक्षरों की चिंता किए।

## कैसे:
```Kotlin
val str = "Hello World!"
val lowercaseStr = str.lowercase()

println(lowercaseStr) // prints "hello world!"
```

यहाँ, `lowercase()` फ़ंक्शन सभी अक्षरों को निचले मामले में बदल देती है। उसके बाद हम उसे print करते हैं।

## गहराई में: 
String को lowercase में कन्वर्ट करने के विचारण का इतिहास समय समय पर बदलता रहा है। पहले `toLowerCase()` फ़ंक्शन का उपयोग किया जाता था, लेकिन अब `lowercase()` Kotlin 1.5.0 के साथ आया है। वैकल्पिक तंत्र डिफ़ॉल्ट के रूप में `Locale` आधारित कार्य कर सकते हैं, जैसे `str.lowercase(Locale.getDefault())`। आंतरिक रूप में, यह फ़ंक्शन अक्षरों के यूनिकोड मान को मोडिफ़ाई करके काम करता है।

## अतिरिक्त जानकारी:
- [Kotlin Documentation : String functions](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/lowercase.html)
- [GeeksforGeeks : Kotlin String Methods](https://www.geeksforgeeks.org/kotlin-string/)
- [Stackoverflow Thread on toLowerCase()](https://stackoverflow.com/questions/11520244/kotlin-string-tolowercase-locale-is-mailformed)