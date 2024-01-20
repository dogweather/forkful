---
title:                "एक तारीख को स्ट्रिंग में परिवर्तित करना"
html_title:           "Java: एक तारीख को स्ट्रिंग में परिवर्तित करना"
simple_title:         "एक तारीख को स्ट्रिंग में परिवर्तित करना"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
दिनांक को स्ट्रिंग में बदलने से संदर्भित, हम एक विशेष तारीख को या तो कस्टम फ़ॉर्मेट में बदलते हैं या इसे इंटरनेट पर एक्सपोर्ट करते हैं। यह मानव यातायात और पठन के लिए आसान बनाने के लिए किया जाता है। 

## कैसे:
यहाँ एक सादा सा Kotlin कोड स्निपेट है जो करंट (current) तारीख को ISO-8601 फ़ॉर्मेट में स्ट्रिंग में बदलता है :

```Kotlin
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

fun main() {
    val current = LocalDateTime.now()
    val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'")
    val formatted = current.format(formatter)
    
    println("Current Date and Time is: $formatted")
}
```
जब आप इसे रन(run) करेंगे, आपको इस तरह का आउटपुट(output) मिलेगा:

```Kotlin
'2022-01-01T12:34:56.789Z'
```

## गहराई में:
इतिहासिक संदर्भ में, जावा में तारीखों को स्ट्रिंग में बदलने का फंक्शन SimpleDateFormat class के साथ आता था। लेकिन Kotlin में, इसे कई तरह से किया जा सकता है जैसे LocalDateTime class के साथ।

विकल्पों में, आप `SimpleDateFormat` का उपयोग कर सकते हैं, जो जावा 7 में उपलब्ध था, लेकिन यह थ्रेड-सेफ(thread-safe) नहीं है। कोटलिन में, उन्होंने इसे थ्रेड-सेफ बनाने का प्रयास किया है।

चूंकि अब तक हम Kotlin का उपयोग कर रहे हैं, हमें जितना संभव हो सके LocalDateTime का उपयोग करना चाहिए क्योंकि इसे कई तरह से कस्टमाइज़ किया जा सकता है, और यह भी थ्रेड-सेफ है।

## देखे भी:
ISO 8601 के बारे में अधिक जानकारी के लिए, पढ़ें: [ISO 8601 - Wikipedia](https://en.wikipedia.org/wiki/ISO_8601)

Kotlin दस्तावेज़ीकरण के लिए, जाएं: [Kotlin Documentation - LocalDate](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/-local-date/index.html)