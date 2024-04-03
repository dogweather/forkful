---
date: 2024-01-20 17:41:33.691424-07:00
description: "How to (\u0915\u0948\u0938\u0947): ."
lastmod: '2024-03-13T22:44:52.292465-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u0905\u0938\u094D\u0925\u093E\u092F\u0940 \u092B\u093E\u0907\u0932 \u092C\
  \u0928\u093E\u0928\u093E"
weight: 21
---

## How to (कैसे):
```Kotlin
import java.io.File
import java.nio.file.Files

fun main() {
    // अस्थायी फ़ाइल बनाने के लिए
    val tempFile: File = Files.createTempFile(null, ".tmp").toFile()
    
    println("अस्थायी फ़ाइल बनायी गयी: ${tempFile.absolutePath}")

    // कुछ काम करने के बाद
    tempFile.deleteOnExit() // प्रोग्राम बंद होने पर फ़ाइल खुद हट जाएगी
}

// नमूना आउटपुट
// अस्थायी फ़ाइल बनायी गयी: /tmp/1234567890.tmp
```

## Deep Dive (गहराई में जानकारी):
अस्थायी फाइलें सबसे पहले यूनिक्स सिस्टम्स में उपयोग की गई थीं, जहाँ `/tmp` डायरेक्टरी में फ़ाइलें संग्रह की जाती थीं। आज भी, अधिकांश ऑपरेटिंग सिस्टम्स टेम्पोररी फ़ाइल स्टोरेज की पेशकश करते हैं। अल्टरनेटिव्स में अस्थायी मेमोरी, डाटाबेस या कैशिंग मैकेनिज्म्स शामिल हैं। `createTempFile` मेथड JVM (Java Virtual Machine) पर निर्भर करता है और उसे प्लेटफ़ॉर्म निरपेक्ष बनाता है। इम्प्लीमेंटेशन संबंधी विवरण में यह भी शामिल है कि मोस्ट सिस्टम्स गारंटी देते हैं कि फ़ाइल यूनिक होगी और सेफ राइट्स के साथ बनाई गई होगी।

## See Also (और भी देखें):
- [Kotlin Official Documentation - Working with Files](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/)
- [Oracle Java Docs – Class Files](https://docs.oracle.com/javase/8/docs/api/java/nio/file/Files.html)
