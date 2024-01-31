---
title:                "स्ट्रिंग से दिनांक पार्स करना"
date:                  2024-01-20T15:37:33.382833-07:00
html_title:           "Arduino: स्ट्रिंग से दिनांक पार्स करना"
simple_title:         "स्ट्रिंग से दिनांक पार्स करना"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
तारीखों को पार्स करना मतलब है तारीख के डेटा को स्ट्रिंग से निकालना। प्रोग्रामर इसे इसलिए करते हैं क्योंकि यूजर इनपुट और डेटा स्टोरेज कई बार स्ट्रिंग फॉरमैट में होता है और उसे डेट ऑब्जेक्ट में बदलना पड़ता है।

## How to (कैसे करें):
```Kotlin
import java.time.LocalDate
import java.time.format.DateTimeFormatter

fun main() {
    val dateString = "15-04-2023"
    val formatter = DateTimeFormatter.ofPattern("dd-MM-yyyy")
    val date = LocalDate.parse(dateString, formatter)
    println(date)  // Output: 2023-04-15
}
```

## Deep Dive (गहराई से समझें):
पहले प्रोग्रामर SimpleDateFormat का इस्तेमाल करते थे। अब, नए API, java.time (जसे LocalDateTime और LocalDate) है, जो थ्रेड सेफ और फ्लेक्सिबल है। जावा 8 से पहले तारीखों को पार्स करना थोड़ा जटिल था, पर अब LocalDate और DateTimeFormatter का कॉम्बिनेशन आपको आसानी से तारीख पार्स करने देता है।

अल्टरनेटिव्स में कोटलिन एक्सटेंशन्स और थर्ड-पार्टी लाइब्रेरीज़ भी हैं, जैसे कि Joda-Time (जिसका प्रयोग अब कम होता है)।

## See Also (और भी जानकारी):
- [Kotlin Official Documentation](https://kotlinlang.org/docs/home.html)
- [Java 8 Date/Time API Guide](https://www.oracle.com/technical-resources/articles/java/jf14-date-time.html)
- [DateTimeFormatter JavaDoc](https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html)
