---
title:                "डायरेक्टरी का अस्तित्व जाँचना"
date:                  2024-01-20T14:57:54.507285-07:00
html_title:           "Elm: डायरेक्टरी का अस्तित्व जाँचना"
simple_title:         "डायरेक्टरी का अस्तित्व जाँचना"

category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
डायरेक्टरी का अस्तित्व जांचना यह सुनिश्चित करता है कि एक निश्चित पथ पर फोल्डर मौजूद है या नहीं। प्रोग्रामर्स इसे फाइल ऑपरेशन से पहले करते हैं ताकि एरर्स से बचा जा सके और डेटा हानि को रोका जा सके।

## How to (कैसे करें):
```kotlin
import java.nio.file.Files
import java.nio.file.Paths

fun main() {
    val directoryPath = Paths.get("/उदाहरण/पथ")
     
    if (Files.exists(directoryPath)) {
        println("डायरेक्टरी मौजूद है।")
    } else {
        println("डायरेक्टरी नहीं मिली।")
    }
}
```

```संभावित आउटपुट:
डायरेक्टरी मौजूद है।
या
डायरेक्टरी नहीं मिली।
```

## Deep Dive (गहराई से जानकारी):
जब फाइल सिस्टम्स की बात आती है, तो यह जानना जरुरी है कि फाइल या डायरेक्टरी का अस्तित्व होता है या नहीं। यह जांच जावा NIO पैकेज (`java.nio.file`) के साथ करना आसान है, जो कि जावा 7 से उपलब्ध है और Kotlin में भी उपयोग किया जा सकता है। विकल्प के रूप में, `File` क्लास (`java.io.File`) का इस्तेमाल करके भी चेक किया जा सकता है, लेकिन `Files` API अधिक आधुनिक और लचीली है। जब डायरेक्टरी की जाँच करते हैं, तो प्रोग्रामर्स को सुरक्षा मुख्यालयों का भी ख्याल रखना चाहिए - उदाहरण के लिए, 'सिम्बॉलिक लिंक्स' से होने वाले संभावित सुरक्षा जोखिम।

## See Also (और जानकारी के लिए):
- [Kotlin Documentation](https://kotlinlang.org/docs/home.html)
- [java.nio.file.Files](https://docs.oracle.com/javase/8/docs/api/java/nio/file/Files.html)
- [java.io.File](https://docs.oracle.com/javase/7/docs/api/java/io/File.html)
