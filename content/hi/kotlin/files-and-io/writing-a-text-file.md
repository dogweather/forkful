---
title:                "एक टेक्स्ट फ़ाइल लिखना"
aliases:
- /hi/kotlin/writing-a-text-file.md
date:                  2024-02-03T19:28:58.865823-07:00
model:                 gpt-4-0125-preview
simple_title:         "एक टेक्स्ट फ़ाइल लिखना"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/writing-a-text-file.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?
कोटलिन में एक टेक्स्ट फ़ाइल लिखना एक फ़ाइल बनाने और उसमें पाठ सामग्री डालने की प्रक्रिया है, जो डेटा संग्रहित करने, लॉगिंग, या विन्यास सेटिंग्स के लिए एक सामान्य कार्य है। प्रोग्रामर इसे अस्थायी मेमोरी स्थान के बाहर डेटा को सहेजने और हेरफेर करने के लिए करते हैं, सत्रों के पार स्थायित्व सुनिश्चित करते हुए।

## कैसे करें:
कोटलिन फ़ाइलों में लिखने के लिए एक सीधी पद्धति प्रदान करता है, मानक पुस्तकालय का लाभ उठाते हुए बिना अतिरिक्त तृतीय-पक्ष पुस्तकालयों की आवश्यकता के। यहाँ एक सरल उदाहरण है:

```kotlin
import java.io.File

fun main() {
    val textToWrite = "Hello, Kotlin file writing!"
    File("example.txt").writeText(textToWrite)
}
```
यह कोड स्निपेट प्रोजेक्ट की रूट निर्देशिका में "example.txt" नामक एक फ़ाइल बनाता है और उसमें `Hello, Kotlin file writing!` स्ट्रिंग लिखता है। अगर फ़ाइल पहले से मौजूद है, तो वह अधिलेखित हो जाएगी।

एक फ़ाइल में अधिक नियंत्रित तरीके से जोड़ने या बड़ी मात्रा में डेटा लिखने के लिए, आप `appendText` या `bufferedWriter()` का उपयोग कर सकते हैं:

```kotlin
import java.io.File

fun appendToFile() {
    val moreText = "Appending more text."
    File("example.txt").appendText(moreText)
}

fun writeWithBufferedWriter() {
    val largeText = "Large amounts of text...\nOn multiple lines."
    File("output.txt").bufferedWriter().use { out ->
        out.write(largeText)
    }
}

fun main() {
    appendToFile() // मौजूदा फ़ाइल में टेक्स्ट जोड़ता है
    writeWithBufferedWriter() // बड़ी टेक्स्ट डेटा को कुशलतापूर्वक लिखता है
}
```

`appendToFile` फ़ंक्शन में, हम "example.txt" में इसकी मौजूदा सामग्री को ओवरराइट किए बिना और अधिक टेक्स्ट जोड़ रहे हैं। `writeWithBufferedWriter` फ़ंक्शन बड़ी मात्रा में टेक्स्ट या डेटा लिखने का एक कुशल तरीका प्रदर्शित करता है, विशेष रूप से जब बहुत सारी पंक्तियाँ या बड़ी फ़ाइलों से निपटते समय आई/ओ ऑपरेशंस को कम करने के लिए उपयोगी।

ये उदाहरण कोटलिन में टेक्स्ट फ़ाइलें लिखने के लिए मौलिक कार्यों को कवर करते हैं, फ़ाइल I/O ऑपरेशंस के लिए कोटलिन के मानक पुस्तकालय की सादगी और शक्ति का प्रदर्शन करते हैं।
