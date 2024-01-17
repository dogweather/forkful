---
title:                "दो दिनांकों को तुलना करना"
html_title:           "Kotlin: दो दिनांकों को तुलना करना"
simple_title:         "दो दिनांकों को तुलना करना"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/comparing-two-dates.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
कोटलिन में दो तारीखों को तुलना करना क्या है और क्यों प्रोग्रामर इसे करते हैं, उसके बारे में दो-तीन वाक्यों में बताना। दो तारीखों को तुलना करना एक आम टास्क है जो प्रोग्रामिंग में अधिकांश समय आता है जिससे हम अपने कोड में विभिन्न तारीखों को संपर्क कर सकते हैं।

## कैसे करें:
कोटलिन में दो तारीखों को तुलना करने के लिए निम्नलिखित स्टेप्स फॉलो करें:

1. दो तारीखों को `LocalDate` ऑब्जेक्ट में convert करें।
2. `isBefore()` और `isAfter()` फंक्शन का उपयोग करके दो तारीखों को तुलना करें।
3. `true` या `false` के रूप में output प्राप्त करें।

```kotlin
val date1 = LocalDate.of(2021, 10, 1)
val date2 = LocalDate.of(2021, 10, 10)

println("Is date1 before date2? ${date1.isBefore(date2)}") // Output: true
println("Is date1 after date2? ${date1.isAfter(date2)}") // Output: false
```

## गहराई में जाएं:
1. पहले, हमारे पास तारीखों को तुलना करने के कई तरीके थे। लेकिन अब `LocalDate` टाइम टेबल में एक स्टैंडर्ड तरीका है।
2. यदि हम `LocalDate` के स्थान पर `Calendar` का उपयोग करते हैं, तो हमें मान्यता स्थिति के साथ काम करनी पड़ती है।
3. `LocalDate` क्लास इम्प्लीमेंटिंग `TemporalAccessor` है, जो तारीख को प्रदर्शित करने के लिए विभिन्न मुहूर्ता महत्त्वों को सामग्री में जोड़ता है।

## इसके अलावा देखें:
1. [ओराकल डॉक्युमेंटेशन: `LocalDate` क्लास](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
2. [कोटलिन डॉक्युमेंटेशन: तारीख और समय](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-date-time/)
3. [वीडियो ट्यूटोरियल: `LocalDate` और अन्य कोटलिन डेटा टाइम क्लास](https://youtu.be/czQRPEeG6cI)