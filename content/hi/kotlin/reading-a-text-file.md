---
title:                "टेक्स्ट फ़ाइल पढ़ना"
date:                  2024-01-20T17:55:12.289934-07:00
model:                 gpt-4-1106-preview
simple_title:         "टेक्स्ट फ़ाइल पढ़ना"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)

पाठ फाइल को पढ़ना मतलब उसके अंदर का डेटा निकालना। प्रोग्रामर्स इसे इसलिए करते हैं क्योंकि कई बार कॉन्फ़िगरेशन, डेटा इनपुट या लॉग फाइलों में स्टोर किए जाते हैं।

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