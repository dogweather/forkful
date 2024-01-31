---
title:                "टेक्स्ट फाइल लिखना"
date:                  2024-01-19
html_title:           "Bash: टेक्स्ट फाइल लिखना"
simple_title:         "टेक्स्ट फाइल लिखना"

category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/writing-a-text-file.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

टेक्स्ट फाइल लिखना यानी डेटा या जानकारी को टेक्स्ट रूप में फाइल में सेव करना है। प्रोग्रामर्स यह इसलिए करते हैं ताकि डेटा को बाद में पढ़ा जा सके, शेयर किया जा सके या डेटा बैकअप बनाया जा सके।

## कैसे करें:

```kotlin
import java.io.File

fun main() {
    val data = "यह एक उदाहरण पाठ है।"
    val fileName = "example.txt"

    File(fileName).writeText(data)
    println("$fileName को सेव कर दिया गया है।")
}
```

सैंपल आउटपुट:

```
example.txt को सेव कर दिया गया है।
```

## गहराई से जानकारी:

टेक्स्ट फाइल लिखने की विधि कंप्यूटर प्रोग्रामिंग के शुरुआती दिनों से ही अपनाई जा रही है। कोटलिन इसे आसान बनाता है जैसे `writeText` फंक्शन का उपयोग करके। इसके अल्टरनेटिव में `FileWriter`, `BufferedWriter` जैसी क्लासेज भी होती हैं, जिनके साथ आप अधिक कस्टमाइजेशन कर सकते हैं। `writeText` एक हायर-लेवल फंक्शन है जो इंटर्नली `OutputStreamWriter` का इस्तेमाल करता है।

## और भी सूत्र:

- कोटलिन डॉक्यूमेंटेशन: https://kotlinlang.org/docs/reference/
- फाइल आई/ओ गाइड: https://kotlinlang.org/docs/working-with-io.html
- Stack Overflow कोटलिन टैग: https://stackoverflow.com/questions/tagged/kotlin

इन लिंक्स पर जाकर आप कोटलिन में फाइल्स से जुड़ी अधिक जानकारी प्राप्त कर सकते हैं।
