---
title:                "तारीख को स्ट्रिंग में बदलना"
date:                  2024-01-20T17:37:35.434539-07:00
model:                 gpt-4-1106-preview
simple_title:         "तारीख को स्ट्रिंग में बदलना"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
तारीख को स्ट्रिंग में बदलने से मतलब है दिनांक को ऐसे फॉर्मेट में परिवर्तित करना जो पढ़ने में सरल हो। प्रोग्रामर इसलिए करते हैं ताकि यूजर्स को डेटा अच्छे से समझ में आए और UI में अच्छा दिखे।

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