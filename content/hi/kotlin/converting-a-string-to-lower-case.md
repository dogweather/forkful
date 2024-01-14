---
title:                "Kotlin: एक स्ट्रिंग को निचले स्तर पर रूपांतरित करना"
programming_language: "Kotlin"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## क्यों
कोई भी व्यक्ति एक लॉवर केस स्ट्रिंग को कनवर्ट करने में रुचि रखता होगा क्योंकि यह उनके कोड में अधिक उच्चतमता दर्ज करता है और स्ट्रिंग को एक स्थिर स्ट्रिंग में प्रवर्तित करता है।

## कैसे
"```Kotlin
var string = "Hello World"
println(string.toLowerCase())
```
Output: hello world"

## गहराई तक जाएं 
लवर स्ट्रिंग को लोअर में कनवर्ट करने के लिए विस्तार में जानकारी के लिए हमारे परिचालन के साथ संभावित समस्याओं पर ध्यान देने के लिए हमारी सिस्टम में इस्तेमाल किए जाने वाले अलग-अलग तकनीकों को समझना उचित होगा। कुछ तकनीकों में, मूल स्ट्रिंग को संशोधित नहीं किया जा सकता है और उसे एक नया स्ट्रिंग वस्तु में कॉपी किया जाता है। इसके बाद, मूल स्ट्रिंग को नए स्ट्रिंग के रूप में प्रोसेस किया जाता है और फिर वह लोअर्स केस में रूपांतरित हो जाता है। इस प्रक्रिया को अधिक जानने के लिए, [लिंक] (https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/to-lower-case.html) पर जाएं।

## देखें भी
किसी भी प्रोग्रामिंग भाषा के लिए ओणतुलित स्ट्रिंग में तब्दीली को समझने के लिए ये [लिंक] (https://www.tutorialspoint.com/kotlin/kotlin_strings.htm) और [लिंक] (https://javatpoint.com/kotlin-to) उपयोगी हो सकते हैं।