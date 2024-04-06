---
date: 2024-01-20 17:32:12.214652-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: \u0938\u0948\u0902\
  \u092A\u0932 \u0906\u0909\u091F\u092A\u0941\u091F."
lastmod: '2024-04-05T21:53:54.286612-06:00'
model: gpt-4-1106-preview
summary: ''
title: "\u092D\u0935\u093F\u0937\u094D\u092F \u092F\u093E \u0905\u0924\u0940\u0924\
  \ \u092E\u0947\u0902 \u0924\u093E\u0930\u0940\u0916 \u0915\u0940 \u0917\u0923\u0928\
  \u093E"
weight: 26
---

## कैसे करें:
```kotlin
import java.time.LocalDate
import java.time.Period

fun main() {
    // आज की तिथि प्राप्त करें
    val today = LocalDate.now()

    // 10 दिन बाद की तिथि
    val tenDaysLater = today.plusDays(10)
    println("10 दिन बाद की तिथि: $tenDaysLater")

    // 2 महीने पहले की तिथि
    val twoMonthsBefore = today.minusMonths(2)
    println("2 महीने पहले की तिथि: $twoMonthsBefore")

    // किसी खास तिथि में वृद्धि करना
    val customDate = LocalDate.of(2023, 4, 1).plus(Period.of(1, 2, 3))
    println("एक साल, दो महीने और तीन दिन बाद: $customDate")
}
```
सैंपल आउटपुट:
```
10 दिन बाद की तिथि: 2023-04-21
2 महीने पहले की तिथि: 2023-02-09
एक साल, दो महीने और तीन दिन बाद: 2024-06-04
```

## गहन विचार:
जावा 8 से पहले, तिथि और समय की गणना `java.util.Date` और `java.util.Calendar` की कक्षाओं का उपयोग की जाती थी, जिनमें अभाव थे। जावा 8 ने `java.time` पैकेज शुरू किया, जिससे हमें `LocalDate`, `LocalTime`, `LocalDateTime`, और `Period` जैसे कक्षाएं मिलीं जो तिथि और समय को सरलता और सुधार के साथ संभालती हैं।

Kotlin भी इन कक्षाओं का उपयोग करता है, और Kotlin के प्रसार कार्यों (extension functions) के ज़रिए, हम इसे और भी सरल बना सकते हैं।

चार्वाक विधान और अंतरराष्ट्रीयकरण हेतु Joda-Time जैसे पुस्तकालय भी उपलब्ध हैं जो जटिल तिथि-समय कार्यों को करने के विकल्प प्रस्तुत करते हैं, किंतु अधिकांश स्थितियों में `java.time` API पर्याप्त है।

## देखें भी:
- Oracle JavaDocs on java.time package: [Java SE Date and Time Guide](https://docs.oracle.com/javase/tutorial/datetime/)
- Joda-Time पुस्तकालय: [Joda-Time](http://www.joda.org/joda-time/)
