---
date: 2024-01-20 17:53:22.125175-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) Debugging\
  \ \u0938\u0947 \u092A\u0939\u0932\u0947, programmers \u0932\u0949\u0917 \u092B\u093E\
  \u0907\u0932\u094D\u0938 \u092F\u093E \u0915\u0902\u0938\u094B\u0932 \u092E\u0947\
  \u0902 \u092A\u094D\u0930\u093F\u0902\u091F \u0915\u0930\u0915\u0947 \u0905\u092A\
  \u0928\u093E \u0915\u094B\u0921 \u091A\u0947\u0915 \u0915\u0930\u0924\u0947 \u0925\
  \u0947\u0964 \u091C\u092C IDEs \u0915\u093E \u0909\u0926\u092F \u0939\u0941\u0906\
  , \u0924\u094B debuggers \u0915\u093E \u0907\u0938\u094D\u0924\u0947\u092E\u093E\
  \u0932\u2026"
lastmod: '2024-04-05T22:51:06.951300-06:00'
model: gpt-4-1106-preview
summary: "(\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) Debugging \u0938\u0947\
  \ \u092A\u0939\u0932\u0947, programmers \u0932\u0949\u0917 \u092B\u093E\u0907\u0932\
  \u094D\u0938 \u092F\u093E \u0915\u0902\u0938\u094B\u0932 \u092E\u0947\u0902 \u092A\
  \u094D\u0930\u093F\u0902\u091F \u0915\u0930\u0915\u0947 \u0905\u092A\u0928\u093E\
  \ \u0915\u094B\u0921 \u091A\u0947\u0915 \u0915\u0930\u0924\u0947 \u0925\u0947\u0964\
  \ \u091C\u092C IDEs \u0915\u093E \u0909\u0926\u092F \u0939\u0941\u0906, \u0924\u094B\
  \ debuggers \u0915\u093E \u0907\u0938\u094D\u0924\u0947\u092E\u093E\u0932 \u092D\
  \u0940 \u092C\u0922\u093C \u0917\u092F\u093E \u091C\u093F\u0938\u0938\u0947 \u0915\
  \u094B\u0921 \u0915\u094B step by step \u091A\u0932\u093E\u0915\u0930 \u0926\u0947\
  \u0916\u093E \u091C\u093E \u0938\u0915\u0924\u093E \u0939\u0948\u0964 \u092B\u093F\
  \u0930 \u092D\u0940, print statements \u0906\u091C \u092D\u0940 \u092E\u0939\u0924\
  \u094D\u0935\u092A\u0942\u0930\u094D\u0923 \u0939\u0948\u0902 \u0915\u094D\u092F\
  \u094B\u0902\u0915\u093F \u0935\u0947 simple \u0939\u0948\u0902 \u0914\u0930 \u0915\
  \u092D\u0940-\u0915\u092D\u0940 debuggers \u0938\u0947 \u091C\u094D\u092F\u093E\u0926\
  \u093E \u091C\u0932\u094D\u0926\u0940 \u0938\u092E\u0938\u094D\u092F\u093E \u0915\
  \u094B \u0938\u093E\u092E\u0928\u0947 \u0932\u093E \u0926\u0947\u0924\u0947 \u0939\
  \u0948\u0902\u0964 Kotlin \u092E\u0947\u0902 `println()` \u0914\u0930 `print()`\
  \ \u0938\u094D\u091F\u0948\u0902\u0921\u0930\u094D\u0921 \u0906\u0909\u091F\u092A\
  \u0941\u091F \u0938\u094D\u091F\u094D\u0930\u0940\u092E \u0915\u0947 \u0932\u093F\
  \u090F \u0939\u0948\u0902, \u091C\u092C\u0915\u093F `System.err.println()` \u0938\
  \u094D\u091F\u0948\u0902\u0921\u0930\u094D\u0921 \u090F\u0930\u0930 \u0938\u094D\
  \u091F\u094D\u0930\u0940\u092E \u0915\u0947 \u0932\u093F\u090F \u0939\u0948\u0964\
  ."
title: "\u0921\u0940\u092C\u0917 \u0906\u0909\u091F\u092A\u0941\u091F \u092A\u094D\
  \u0930\u093F\u0902\u091F \u0915\u0930\u0928\u093E"
weight: 33
---

## How to: (कैसे करें:)
```kotlin
fun main() {
    val name = "दुनिया"
    println("नमस्ते, $name!")  // Console पर मैसेज प्रिंट करने के लिए

    val error = Exception("कुछ गड़बड़ हो गई")
    System.err.println("एरर: $error")  // Error stream में मैसेज प्रिंट करने के लिए
}
```
Sample output:
```
नमस्ते, दुनिया!
एरर: java.lang.Exception: कुछ गड़बड़ हो गई
```

## Deep Dive (गहराई में जानकारी):
Debugging से पहले, programmers लॉग फाइल्स या कंसोल में प्रिंट करके अपना कोड चेक करते थे। जब IDEs का उदय हुआ, तो debuggers का इस्तेमाल भी बढ़ गया जिससे कोड को step by step चलाकर देखा जा सकता है। फिर भी, print statements आज भी महत्वपूर्ण हैं क्योंकि वे simple हैं और कभी-कभी debuggers से ज्यादा जल्दी समस्या को सामने ला देते हैं। Kotlin में `println()` और `print()` स्टैंडर्ड आउटपुट स्ट्रीम के लिए हैं, जबकि `System.err.println()` स्टैंडर्ड एरर स्ट्रीम के लिए है।

## See Also (और जानकारी के लिए):
- [Stack Overflow Discussions on Kotlin Debug Techniques](https://stackoverflow.com/questions/tagged/kotlin+debugging)
- [JetBrains' IntelliJ IDEA Debugger](https://www.jetbrains.com/help/idea/debugging-your-first-java-application.html)
