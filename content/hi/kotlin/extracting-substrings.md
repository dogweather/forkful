---
title:                "सबस्ट्रिंग्स निकालना"
html_title:           "Clojure: सबस्ट्रिंग्स निकालना"
simple_title:         "सबस्ट्रिंग्स निकालना"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/extracting-substrings.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
सबस्ट्रिंग्स निकालना एक विशेष टेक्स्ट को एक बड़े टेक्स्ट से अलग करना होता है। प्रोग्रामर्स इसे तब करते हैं जब उन्हें सिर्फ एक विशेष भाग की जरूरत होती है, न कि पूरे टेक्स्ट की।  

## कैसे करें:
सबस्ट्रिंग निकालने के लिए Kotlin में कुछ विशेष फंक्शन्स होते हैं। इनके प्रयोग की उदाहरण दीजिएगी:

```Kotlin
// यह मूल स्ट्रिंग है:
val str = "Hello programmers"

// आप सबस्ट्रिंग इस प्रकार निकाल सकते हैं:
val sub = str.substring(6, 17)

println(sub) // Output: "programmers"
```

## गहरा डाइव:
- **ऐतिहासिक प्रसंग**: Kotlin में सबस्ट्रिंग निकालने की क्षमता को खासकर डेटा प्रसंस्करण की जरूरतों को ध्यान में रखकर जोड़ा गया है।
- **विकल्प**: 'slice' फ़ंक्शन भी सबस्ट्रिंग के लिए एक विकल्प हो सकता है। यह चारों ओर से विशिष्ट स्थान तक स्ट्रिंग प्राप्त करता है।
- **क्रियान्वयन विवरण**: `substring` फ़ंक्शन स्वयं में एक शुरुआत और अंत मार्कर के द्वारा संदर्भित स्ट्रिंग का एक हिस्सा निकालता है। यह अक्सर समय की जटिलता O(N) होती है, जहां N स्ट्रिंग की लंबाई है।

## देखें भी:
- [Kotlin डॉक्यूमेंटेशन: स्ट्रिंग](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/)
- [Kotlin Tutorial: सबस्ट्रिंग और स्लाइसिंग](https://www.programiz.com/kotlin-programming/string)
- [GeeksForGeeks: Kotlin substring() Functions](https://www.geeksforgeeks.org/kotlin-string-substring/)