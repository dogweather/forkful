---
title:                "डायरेक्टरी मौजूद है या नहीं जाँचना"
aliases: - /hi/kotlin/checking-if-a-directory-exists.md
date:                  2024-02-03T19:08:13.115210-07:00
model:                 gpt-4-0125-preview
simple_title:         "डायरेक्टरी मौजूद है या नहीं जाँचना"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?
Kotlin में एक निर्देशिका के अस्तित्व की जांच का मतलब एक निर्दिष्ट पथ पर एक निर्देशिका की उपस्थिति की पुष्टि करना है। प्रोग्रामर ऐसे कार्य को इसलिए करते हैं ताकि गलतियों से बचा जा सके, जैसे कि किसी अस्तित्व में न होने वाले निर्देशिका से पढ़ने या लिखने की कोशिश करना, इससे एप्लीकेशनों के भीतर फ़ाइल हैंडलिंग और डेटा प्रबंधन सुगम होता है।

## कैसे करें:
JVM पर चलने वाला Kotlin, फ़ाइल ऑपरेशनों के लिए जावा फ़ाइल API का लाभ उठाता है, जिससे निर्देशिका अस्तित्व की जाँच सीधी और सरल हो जाती है। यहाँ एक मूल उदाहरण है:

```kotlin
import java.io.File

fun main() {
    val path = "/path/to/directory"
    val directory = File(path)

    if (directory.exists() && directory.isDirectory) {
        println("Directory exists: $path")
    } else {
        println("Directory does not exist: $path")
    }
}
```
उदाहरण आउटपुट, मानते हुए कि निर्देशिका मौजूद है:
```
Directory exists: /path/to/directory
```
और अगर वह मौजूद नहीं है:
```
Directory does not exist: /path/to/directory
```

एक Kotlin परियोजना में, आप शायद वेब एप्लीकेशनों के लिए Ktor या असिंक्रोनस प्रोग्रामिंग के लिए kotlinx.coroutines जैसी Kotlin-विशिष्ट लाइब्रेरीज़ या फ्रेमवर्क्स के साथ भी अक्सर काम करते हैं। हालाँकि, एक निर्देशिका के अस्तित्व को जांचने के लिए, प्रदर्शित Java `File` API आमतौर पर पर्याप्त और व्यापक रूप से प्रयोग की जाती है, क्योंकि Kotlin की जावा के साथ सहयोग करने की क्षमता के कारण। इस विशेष कार्य के लिए कोई तीसरे पक्ष की लाइब्रेरी की आवश्यकता नहीं होती, इसे अन्य प्रोग्रामिंग भाषाओं से Kotlin में संक्रमण कर रहे नौसिखुओं के लिए सुलभ और सीधा बनाते हैं।
