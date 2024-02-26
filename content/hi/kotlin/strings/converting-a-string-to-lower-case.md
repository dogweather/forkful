---
date: 2024-01-20 17:38:56.724686-07:00
description: "String \u0915\u094B lower case \u092E\u0947\u0902 convert \u0915\u0930\
  \u0928\u093E \u092E\u0924\u0932\u092C \u0909\u0938\u0915\u0947 \u0939\u0930 character\
  \ \u0915\u094B \u091B\u094B\u091F\u0947 \u0905\u0915\u094D\u0937\u0930\u094B\u0902\
  \ \u092E\u0947\u0902 \u092C\u0926\u0932\u0928\u093E \u0939\u0948\u0964 \u092F\u0939\
  \ \u0915\u093E\u092E \u0905\u0915\u094D\u0938\u0930 \u0907\u0938\u0932\u093F\u090F\
  \ \u0915\u093F\u092F\u093E \u091C\u093E\u0924\u093E \u0939\u0948 \u0924\u093E\u0915\
  \u093F text \u0915\u0940 comparison uniform\u2026"
lastmod: '2024-02-25T18:49:49.436459-07:00'
model: gpt-4-1106-preview
summary: "String \u0915\u094B lower case \u092E\u0947\u0902 convert \u0915\u0930\u0928\
  \u093E \u092E\u0924\u0932\u092C \u0909\u0938\u0915\u0947 \u0939\u0930 character\
  \ \u0915\u094B \u091B\u094B\u091F\u0947 \u0905\u0915\u094D\u0937\u0930\u094B\u0902\
  \ \u092E\u0947\u0902 \u092C\u0926\u0932\u0928\u093E \u0939\u0948\u0964 \u092F\u0939\
  \ \u0915\u093E\u092E \u0905\u0915\u094D\u0938\u0930 \u0907\u0938\u0932\u093F\u090F\
  \ \u0915\u093F\u092F\u093E \u091C\u093E\u0924\u093E \u0939\u0948 \u0924\u093E\u0915\
  \u093F text \u0915\u0940 comparison uniform\u2026"
title: "\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0915\u094B \u091B\u094B\
  \u091F\u0947 \u0905\u0915\u094D\u0937\u0930\u094B\u0902 \u092E\u0947\u0902 \u092A\
  \u0930\u093F\u0935\u0930\u094D\u0924\u093F\u0924 \u0915\u0930\u0928\u093E"
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
