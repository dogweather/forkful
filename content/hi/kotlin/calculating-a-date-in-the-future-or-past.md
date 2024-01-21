---
title:                "भविष्य या अतीत में तारीख की गणना"
date:                  2024-01-20T17:32:12.214652-07:00
model:                 gpt-4-1106-preview
simple_title:         "भविष्य या अतीत में तारीख की गणना"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
तिथि की गणना का मतलब है भविष्य या अतीत में किसी तिथि को ढूँढना। प्रोग्रामर्स ऐसा इसलिए करते हैं क्योंकि कभी-कभी अनुप्रयोगों को भविष्य की तारीखों की आवश्यकता होती है—जैसे किस्तों की गणना करना, या अतीत की तिथियों को जानना।

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