---
title:                "वर्तमान तारीख प्राप्त करना"
date:                  2024-01-20T15:15:48.518024-07:00
html_title:           "C: वर्तमान तारीख प्राप्त करना"
simple_title:         "वर्तमान तारीख प्राप्त करना"

category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
प्रोग्राम में वर्तमान तारीख जानना आवश्यक हो सकता है, जैसे लॉग्स के लिए या किसी कार्यक्रम घटना का समय दर्ज करने के लिए। यह एप्लिकेशन को अधिक प्रासंगिक और प्रयोगकर्ता-अनुरूप बनाता है।

## How to: (कैसे करें:)
Kotlin में वर्तमान तारीख प्राप्त करना बेहद आसान है। नीचे दिए गए कोड स्निपेट का प्रयोग करें:

```Kotlin
import java.time.LocalDate

fun main() {
    val today = LocalDate.now()
    println("आज की तारीख है: $today")
}
```

जब आप इस कोड को चलाएंगे, आउटपुट कुछ ऐसा दिखाई देगा (तारीख आपके वर्तमान दिन पर आधारित होगी):

```
आज की तारीख है: 2023-03-25
```

## Deep Dive (गहराई में जानकारी):
तारीख और समय को हैंडल करने के लिए Java 8 से पहले `java.util.Date` और `java.util.Calendar` का इस्तेमाल होता था। हालांकि, इनमें कुछ समस्याएं थीं, जैसे असुरक्षित थ्रेड और डिज़ाइन में पेचीदगियां। Java 8 में `java.time` पैकेज इस समस्या का समाधान लेकर आया और इसे Kotlin में भी अपनाया गया। 

`LocalDate.now()` का प्रयोग करना इसलिए बेहतर है क्योंकि यह टाइम-ज़ोन से स्वतंत्र होता है और केवल दिनांक प्रदान करता है। अगर आपको समय ज़ोन के साथ वर्तमान दिनांक और समय चाहिए, तो `ZonedDateTime.now()` या `OffsetDateTime.now()` का इस्तेमाल कर सकते हैं।

## See Also (और जानकारी):

- Kotlin डॉक्युमेंटेशन: [Kotlin Documentation](https://kotlinlang.org/docs/reference/)
- java.time पैकेज: [Oracle java.time package](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- Kotlin और डेटा हैंडलिंग: [Handling Dates in Kotlin](https://medium.com/@kashifmin/handling-dates-in-kotlin-978c9ae1b5e5)
