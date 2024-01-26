---
title:                "स्ट्रिंग को छोटे अक्षरों में परिवर्तित करना"
date:                  2024-01-20T17:38:56.724686-07:00
model:                 gpt-4-1106-preview
simple_title:         "स्ट्रिंग को छोटे अक्षरों में परिवर्तित करना"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

String को lower case में convert करना मतलब उसके हर character को छोटे अक्षरों में बदलना है। यह काम अक्सर इसलिए किया जाता है ताकि text की comparison uniform तरीके से हो सके, जैसे कि user input और database values के बीच।

## कैसे करें:

Kotlin में, String को lower case में convert करने के लिए `.toLowerCase()` या `.lowercase()` function का इस्तेमाल करें।

```kotlin
fun main() {
    val originalString = "नमस्ते, कोटलिन!"
    val lowerCaseString = originalString.lowercase()
    
    println(lowerCaseString) // आउटपुट: "नमस्ते, कोटलिन!"
}
```
## गहराई से जानकारी:

पहले, `.toLowerCase()` का इस्तेमाल होता था, पर Kotlin 1.5 से `.lowercase()` अधिक पसंद किया जाने लगा है क्योंकि यह locale-sensitive है। Locale मतलब प्रोग्राम जो भी भाषा या क्षेत्रीय सेटिंग्स को मानता है। `.lowercase(Locale)` संस्करण का इस्तेमाल करके हम विशिष्ट भाषा के नियमों के अनुसार अक्षरों को छोटा कर सकते हैं।

इसके alternatives में regular expressions और manual iterations शामिल हैं, पर वे अधिक कोड और जटिलता लाते हैं। Kotlin के `.lowercase()` function का इस्तेमाल करने से यह काम आसान और प्रभावी रहता है।

इसके आंतरिक implementation में, Kotlin JVM (Java Virtual Machine) पर चलते हुए Java के `String.toLowerCase()` method का उपयोग करता है। लेकिन Kotlin/Native या Kotlin/JS जैसे पर्यावरणों में, यह अपने निजी implementation पर निर्भर करता है।

## और जानकारी के लिए:

- Kotlin डॉक्यूमेंटेशन: [Strings](https://kotlinlang.org/docs/reference/basic-types.html#strings)
- Oracle Java डॉक्यूमेंटेशन: [toLowerCase()](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html#toLowerCase())
