---
title:                "एक पाठ फ़ाइल पढ़ना"
html_title:           "Bash: एक पाठ फ़ाइल पढ़ना"
simple_title:         "एक पाठ फ़ाइल पढ़ना"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/reading-a-text-file.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
वाचन (Reading) का अर्थ होता है - फ़ाइल में संग्रहीत डेटा को पढ़ना। कई बार, प्रोग्रामर्स को उनके एप्लिकेशन्स में लोकल्य या सर्वर पर मौजूद फ़ाइलों को पढ़ने की आवश्यकता होती है।

## कैसे:
आइए देखें कि कोटलिन में टेक्स्ट फ़ाइल को कैसे पढ़ा जा सकता है:

```Kotlin
import java.io.File

fun main() {
    val txtFile = File("test.txt")
    txtFile.forEachLine { println(it) }
}
```

इस कोड का उद्देश्य "test.txt" नामक फाइल को पढ़ना है और हर लाइन को प्रिंट करना।

## गहराई में जाने:
1. ऐतिहासिक संदर्भ: पहले, फ़ाइलों को पढ़ने के लिए `FileInputStream` और `BufferedReader` का उपयोग किया जाता था। लेकिन, Kotlin ने इसे आसान और सुविधाजनक बना दिया है।

2. विकल्प: अगर आपको एक से अधिक फ़ाइलों को पढ़ने की आवश्यकता है, तो `FileTreeWalk` और `filter` functions का उपयोग कर सकते हैं।

3. क्रियान्वयन विवरण: `forEachLine` क्रिया हर लाइन को एक बार में प्राप्त करती है, जिससे हीप मेमोरी का उपयोग कम होता है।

## देखें भी:
1. [कोटलिन डॉक्यूमेंटेशन](https://kotlinlang.org/docs/tutorials/kotlin-for-py/file-io.html)
2. [कोटलिन के File Reading के उदाहरण](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/read-text.html)