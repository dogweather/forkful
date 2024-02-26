---
date: 2024-01-20 17:53:22.125175-07:00
description: "Debug output print \u0915\u0930\u0928\u093E \u090F\u0915 \u0910\u0938\
  \u093E \u0924\u0930\u0940\u0915\u093E \u0939\u0948 \u091C\u093F\u0938\u0938\u0947\
  \ \u0915\u094B\u0921 \u092E\u0947\u0902 \u0915\u094D\u092F\u093E \u0939\u094B \u0930\
  \u0939\u093E \u0939\u0948 \u092F\u0939 \u0939\u092E\u0947\u0902 \u092A\u0924\u093E\
  \ \u091A\u0932\u0924\u093E \u0939\u0948\u0964 Programmers \u0907\u0938\u0947 \u0907\
  \u0938\u0932\u093F\u090F \u0915\u0930\u0924\u0947 \u0939\u0948\u0902 \u0924\u093E\
  \u0915\u093F \u0935\u0947 bugs \u0915\u094B \u092A\u0939\u091A\u093E\u0928 \u0938\
  \u0915\u0947\u0902 \u0914\u0930 \u0915\u094B\u0921\u2026"
lastmod: '2024-02-25T18:49:49.463303-07:00'
model: gpt-4-1106-preview
summary: "Debug output print \u0915\u0930\u0928\u093E \u090F\u0915 \u0910\u0938\u093E\
  \ \u0924\u0930\u0940\u0915\u093E \u0939\u0948 \u091C\u093F\u0938\u0938\u0947 \u0915\
  \u094B\u0921 \u092E\u0947\u0902 \u0915\u094D\u092F\u093E \u0939\u094B \u0930\u0939\
  \u093E \u0939\u0948 \u092F\u0939 \u0939\u092E\u0947\u0902 \u092A\u0924\u093E \u091A\
  \u0932\u0924\u093E \u0939\u0948\u0964 Programmers \u0907\u0938\u0947 \u0907\u0938\
  \u0932\u093F\u090F \u0915\u0930\u0924\u0947 \u0939\u0948\u0902 \u0924\u093E\u0915\
  \u093F \u0935\u0947 bugs \u0915\u094B \u092A\u0939\u091A\u093E\u0928 \u0938\u0915\
  \u0947\u0902 \u0914\u0930 \u0915\u094B\u0921\u2026"
title: "\u0921\u0940\u092C\u0917 \u0906\u0909\u091F\u092A\u0941\u091F \u092A\u094D\
  \u0930\u093F\u0902\u091F \u0915\u0930\u0928\u093E"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
Debug output print करना एक ऐसा तरीका है जिससे कोड में क्या हो रहा है यह हमें पता चलता है। Programmers इसे इसलिए करते हैं ताकि वे bugs को पहचान सकें और कोड की जानकारी आसानी से समझ सकें।

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
