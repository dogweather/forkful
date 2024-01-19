---
title:                "वर्तमान तारीख प्राप्त करना"
html_title:           "C#: वर्तमान तारीख प्राप्त करना"
simple_title:         "वर्तमान तारीख प्राप्त करना"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/getting-the-current-date.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

वर्तमान तिथि प्राप्त करना यहां परकार का कार्य होता है, जब हम कोड में वर्तमान दिनांक और समय की जानकारी प्राप्त करते हैं। कार्यक्रमकर्ताओं को तिथि समय को पैटर्न में निर्माण करने या जांचने के लिए इसकी जरूरत पड़ती है, जैसे कि लॉग इन विवरण, एपलिकेशन तत्व की स्थिति की ट्रैकिंग, और अधिक। 

## कैसे करें:
वर्तमान दिनांक प्राप्त करने के लिए Kotlin प्रोग्राम का उदाहरण:

यह Kotlin कोड वर्तमान तारीख और समय कैसे प्राप्त करेंगे:

```Kotlin
import java.time.LocalDateTime
fun main() {
    val current = LocalDateTime.now()
    println("Current Date and Time is: $current")
}
```
आउटपुट इस तरह होता है:

```Kotlin
Current Date and Time is: 2022-11-04T09:45:30.123
```

## गहराई में:
1) **ऐतिहासिक प्रसंग**: जावा 8 के पहले, दिनांक और समय वर्ग का सामर्थन कम था और समय मानवांशिक विचारना मुश्किल था। जावा 8 ने LocalDateTime API जारी किया जो यहां सेट किया गया है।

2) **विकल्प**: अन्य कलासेस, जैसे LocalDate (तारीख ही प्राप्त करता है), या LocalTime (केवल समय प्राप्त करता है) भी उपलब्ध हैं। 

3) **कार्यान्वयन विवरण**: LocalDateTime.now() ग्रीनविच मीन टाइम (GMT) में वर्तमान तिथि और समय प्रदान करता है।

## यह भी देखें:

1) [Kotlin Documentation: Date and Time](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/-date-time/index.html)
2) [Baeldung guide on Kotlin Date and Time](https://www.baeldung.com/kotlin/date-time)
3) [Github - Kotlin Examples](https://github.com/JetBrains/kotlin-examples)