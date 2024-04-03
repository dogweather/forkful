---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:15:18.289838-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: Kotlin \u091C\u093E\
  \u0935\u093E 8 \u092E\u0947\u0902 \u092A\u0947\u0936 `java.time` \u092A\u0948\u0915\
  \u0947\u091C \u0915\u0947 \u092E\u093E\u0927\u094D\u092F\u092E \u0938\u0947 \u0926\
  \u093F\u0928\u093E\u0902\u0915 \u092A\u093E\u0930\u094D\u0938\u093F\u0902\u0917\
  \ \u0915\u093E \u0938\u092E\u0930\u094D\u0925\u0928 \u0915\u0930\u0924\u093E \u0939\
  \u0948\u0964 \u092F\u0939\u093E\u0901 `LocalDateTime` \u0914\u0930 \u090F\u0915\
  \ \u0935\u093F\u0936\u093F\u0937\u094D\u091F \u092A\u0948\u091F\u0930\u094D\u0928\
  \ \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\u0915\u0947\u2026"
lastmod: '2024-03-13T22:44:52.275168-06:00'
model: gpt-4-0125-preview
summary: "Kotlin \u091C\u093E\u0935\u093E 8 \u092E\u0947\u0902 \u092A\u0947\u0936\
  \ `java.time` \u092A\u0948\u0915\u0947\u091C \u0915\u0947 \u092E\u093E\u0927\u094D\
  \u092F\u092E \u0938\u0947 \u0926\u093F\u0928\u093E\u0902\u0915 \u092A\u093E\u0930\
  \u094D\u0938\u093F\u0902\u0917 \u0915\u093E \u0938\u092E\u0930\u094D\u0925\u0928\
  \ \u0915\u0930\u0924\u093E \u0939\u0948\u0964 \u092F\u0939\u093E\u0901 `LocalDateTime`\
  \ \u0914\u0930 \u090F\u0915 \u0935\u093F\u0936\u093F\u0937\u094D\u091F \u092A\u0948\
  \u091F\u0930\u094D\u0928 \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\
  \u0915\u0947 \u090F\u0915 \u0938\u0930\u0932 \u0926\u0943\u0937\u094D\u091F\u093F\
  \u0915\u094B\u0923 \u0939\u0948."
title: "\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0938\u0947 \u0924\u093E\
  \u0930\u0940\u0916 \u092A\u093E\u0930\u094D\u0938 \u0915\u0930\u0928\u093E"
weight: 30
---

## कैसे करें:
Kotlin जावा 8 में पेश `java.time` पैकेज के माध्यम से दिनांक पार्सिंग का समर्थन करता है। यहाँ `LocalDateTime` और एक विशिष्ट पैटर्न का उपयोग करके एक सरल दृष्टिकोण है:

```kotlin
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

fun parseDateFromString(dateString: String): LocalDateTime {
    val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")
    return LocalDateTime.parse(dateString, formatter)
}

fun main() {
    val dateString = "2023-04-01 12:00:00"
    val date = parseDateFromString(dateString)
    println(date)  // आउटपुट: 2023-04-01T12:00
}
```

अधिक लचीलेपन के लिए, या APIs जैसे बाहरी स्त्रोतों से दिनांक संभालने के लिए, आप Joda-Time जैसी तृतीय-पक्ष लाइब्रेरी का उपयोग कर सकते हैं (हालांकि `java.time` के रूस्त रहने के साथ यह अब कम आम है)। हालांकि, JDK द्वारा प्रदान की गई आधुनिक दृष्टिकोण से चिपके रहना अधिकतर Kotlin एप्लिकेशनों के लिए पसंदीदा है।

बिना तृतीय-पक्ष लाइब्रेरीज का उपयोग किए, Kotlin में एक दिनांक को पार्स करने के लिए, आप Java 8 से पहले के संस्करणों में या Android API स्तरों में `java.time` समर्थन की कमी के लिए `SimpleDateFormat` क्लास का भी उपयोग कर सकते हैं:

```kotlin
import java.text.SimpleDateFormat

fun parseDateUsingSimpleDateFormat(dateString: String): java.util.Date {
    val formatter = SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
    return formatter.parse(dateString)
}

fun main() {
    val dateString = "2023-04-01 12:00:00"
    val date = parseDateUsingSimpleDateFormat(dateString)
    println(date)  // आउटपुट आपके समय क्षेत्र के आधार पर विभिन्न होगा, उदाहरण के लिए, Sat Apr 01 12:00:00 GMT 2023
}
```

`SimpleDateFormat` के साथ काम करते समय हमेशा समय क्षेत्र सेट करना याद रखें ताकि पार्स की गई दिनांकों में अप्रत्याशित ऑफसेट से बचा जा सके।
