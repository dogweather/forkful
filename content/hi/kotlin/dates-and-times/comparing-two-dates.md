---
title:                "दो तारीखों की तुलना"
aliases:
- /hi/kotlin/comparing-two-dates/
date:                  2024-01-20T17:33:57.298481-07:00
model:                 gpt-4-1106-preview
simple_title:         "दो तारीखों की तुलना"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/comparing-two-dates.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
तिथियों की तुलना मतलब है दो तारीखों को एक-दूसरे के संबंध में देखना - कौन सी पहले है, बाद में है, या क्या दोनों समान हैं। प्रोग्रामर्स इसे शेड्यूलिंग, टाइम-ट्रैकिंग, या वैलिडेशन उद्देश्यों के लिए करते हैं।

## How to: (कैसे करें:)
```kotlin
import java.time.LocalDate

fun main() {
    val date1 = LocalDate.of(2023, 3, 15)
    val date2 = LocalDate.now()

    println("Date 1: $date1")
    println("Date 2: $date2")

    when {
        date1.isBefore(date2) -> println("Date1 पहले हैं Date2 से.")
        date1.isAfter(date2) -> println("Date1 बाद में है Date2 से.")
        else -> println("Date1 और Date2 समान हैं.")
    }
}
```
सैंपल आउटपुट:
```
Date 1: 2023-03-15
Date 2: 2023-04-05
Date1 पहले हैं Date2 से.
```

## Deep Dive (गहराई से जानकारी)
तारीखों की तुलना करना प्रोग्रामिंग में एक सामान्य कार्य है और इतिहास में विभिन्न लाइब्रेरी और फंक्शंस इसके लिए बनाई गई हैं। Kotlin में, `java.time.LocalDate` जैसी नई जावा टाइम API का उपयोग अधिक सुरक्षित और सहज है, इसमें `isBefore()`, `isAfter()`, और `isEqual()` जैसे मेथ

ड्स हैं जो ज़्यादा पठनीय और त्रुटि-मुक्त कोडिंग के दिशा में एक कदम है। इससे पहले, जावा में `java.util.Date` और `java.util.Calendar` थे, पर हाल की API में सुधारों से तिथियों की तुलना सरल और अधिक निश्चित होती जा रही है।

## See Also (इसे भी देखें)
- [Oracle's JavaDocs on LocalDate](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
