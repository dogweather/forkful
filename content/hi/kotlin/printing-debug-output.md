---
title:                "डीबग आउटपुट प्रिंट करना"
date:                  2024-01-20T17:53:22.125175-07:00
model:                 gpt-4-1106-preview
simple_title:         "डीबग आउटपुट प्रिंट करना"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/printing-debug-output.md"
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
