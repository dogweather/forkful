---
title:                "अस्थायी फाइल बनाना"
aliases: - /hi/kotlin/creating-a-temporary-file.md
date:                  2024-01-20T17:41:33.691424-07:00
model:                 gpt-4-1106-preview
simple_title:         "अस्थायी फाइल बनाना"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)

अस्थायी फ़ाइल बनाने का मतलब है ऐसी फ़ाइल जो कुछ समय के लिए ही उपयोग में आती है। प्रोग्रामर्स डेटा अस्थायी तौर पर स्टोर करने, बड़े ऑपरेशन्स के लिए बफरिंग करने, या किसी प्रोसेस के दौरान इंटरमीडिएट रिजल्ट्स संरक्षित करने के लिए इसका इस्तेमाल करते हैं।

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
