---
date: 2024-01-20 17:55:12.289934-07:00
description: "How to (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902): ."
lastmod: '2024-03-13T22:44:52.289047-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u091F\u0947\u0915\u094D\u0938\u094D\u091F \u092B\u093C\u093E\u0907\u0932\
  \ \u092A\u0922\u093C\u0928\u093E"
weight: 22
---

## How to (कैसे करें):
```Kotlin
import java.io.File

fun main() {
    val data = File("example.txt").readText()
    println(data)
}
```

सैंपल आउटपुट:
```
नमस्कार! यह मेरा पहला कोटलिन प्रोग्राम है।
```

## Deep Dive (गहराई से जानकारी):
पाठ फाइलें पढ़ना जावा में `java.io` और `java.nio` पैकेज के जरिए पहले से ही आसान था। कोटलिन में भी, यह `java.io.File` क्लास के एक्सटेंशन फंक्शन का इस्तेमाल करके सरल बन गया है। अलग-अलग तरीके हैं फाइल पढ़ने के जैसे `readLines()`, `forEachLine` जो अलग-अलग सिचुएशन में उपयोगी हैं, जैसे हर लाइन को इंडिविजुअली प्रोसेस करना। `InputStream` और `BufferedReader` यह भी ऑप्शन हैं जब आपको बड़ी फाइल्स को इफिशिएंटली हैंडल करना हो।

## See Also (और भी देखें):
- कोटलिन डॉक्यूमेंटेशन: [https://kotlinlang.org/docs/home.html](https://kotlinlang.org/docs/home.html)
- `java.io.File` क्लास रेफरेंस: [https://docs.oracle.com/javase/7/docs/api/java/io/File.html](https://docs.oracle.com/javase/7/docs/api/java/io/File.html)
