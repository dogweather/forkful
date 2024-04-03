---
date: 2024-01-20 17:37:35.434539-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) ."
lastmod: '2024-03-13T22:44:52.278762-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u0924\u093E\u0930\u0940\u0916 \u0915\u094B \u0938\u094D\u091F\u094D\u0930\
  \u093F\u0902\u0917 \u092E\u0947\u0902 \u092C\u0926\u0932\u0928\u093E"
weight: 28
---

## How to: (कैसे करें:)
```kotlin
import java.text.SimpleDateFormat
import java.util.*

fun main() {
    val date: Date = Date() // आज की तारीख लें
    val formatter = SimpleDateFormat("dd-MM-yyyy HH:mm:ss") // फॉर्मेट सेट करें
    val dateStr = formatter.format(date) // स्ट्रिंग में बदलें

    println(dateStr) // आउटपुट दिखाएँ
}
```
Sample output:
```
05-04-2023 16:45:12
```

## Deep Dive (गहराई से जानकारी):
तारीखों को स्ट्रिंग में बदलने का चलन डाटाबेस और यूजर इंटरफेस की शुरुआत से रहा है। `SimpleDateFormat` जावा में पुराना और प्रचलित तरीका है, लेकिन Kotin में `DateTimeFormatter` जैसे नए लाइब्रेरी भी हैं। जब किसी तारीख़ को स्ट्रिंग में बदलते हैं, तो टाइमज़ोन की सटीकता, लोकलाइजेशन(स्थानीकरण) और फॉर्मैटिंग पैटर्न जैसे पहलुओं को ध्यान में रखना जरूरी होता है। कई बार `Date` की बजाए `Calendar` या `LocalDateTime` क्लासेस इस्तेमाल होते हैं, जो समय क्षेत्र और लोकलाइजेशन को बेहतर संभालते हैं।

## See Also (अधिक जानकारी के लिए):
- [Kotlin Official Documentation](https://kotlinlang.org/docs/reference/)
- [SimpleDateFormat Documentation](https://docs.oracle.com/javase/7/docs/api/java/text/SimpleDateFormat.html)
