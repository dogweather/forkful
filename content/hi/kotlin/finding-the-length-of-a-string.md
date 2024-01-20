---
title:                "स्ट्रिंग की लंबाई पता करना"
html_title:           "C++: स्ट्रिंग की लंबाई पता करना"
simple_title:         "स्ट्रिंग की लंबाई पता करना"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों? (What & Why?)

स्ट्रिंग की लंबाई खोजना मतलब होता है कितने अक्षर उस स्ट्रिंग में हैं। प्रोग्रामर इसे करते हैं क्योंकि यह उन्हें डेटा को संबोधित और प्रतिष्ठापित करने में मदद करता है।

## कैसे करें: (How to:)

Kotlin में एक स्ट्रिंग की लंबाई खोजने के लिए, हम 'length' property का उपयोग करते हैं।

```Kotlin
fun main(args: Array<String>) {
    val str = "हैलो, दुनिया!"
    val len = str.length

    println("स्ट्रिंग की लंबाई: $len")
}
```

ऊपर दिए गए कोड का आउटपुट होगा:

```
स्ट्रिंग की लंबाई: 13
```

## गहन जानकारी: (Deep Dive:)

स्ट्रिंग की लंबाई जानना, कंप्यूटर साइंस की मूल बातों में से एक है। यह अनिवार्य था जब कंप्यूटर राम की सीमा के साथ डील करने के लिए संघर्ष कर रहे थे।

Kotlin में, हमें किन्हीं अन्य कार्यों की तुलना में स्ट्रिंग की लंबाई ज्ञात करने के लिए पहले किसी पुस्तकालय की आवश्यकता नहीं होती। यह भाषा के कोर में इंबेडेड होता है।

यदि आपको एक स्ट्रिंग की लंबाई के बारे में अधिक जानकारी चाहिए तो आप 'str.length()' के स्थान पर 'str.toCharArray().size' का उपयोग कर सकते हैं। 

## और भी देखें: (See Also:)

यदि आप और भी Kotlin प्रोग्रामिंग के बारे में जानना चाहते हैं, तो निम्नलिखित लिंक्स की जांच करें:

1. Official Kotlin documentation: https://kotlinlang.org/docs/reference/

2. Kotlin for Beginners: https://www.programiz.com/kotlin-programming

3. Kotlin Strings: https://www.tutorialkart.com/kotlin/kotlin-string/