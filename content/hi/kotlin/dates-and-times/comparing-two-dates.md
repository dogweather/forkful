---
date: 2024-01-20 17:33:57.298481-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) \u0938\u0948\
  \u0902\u092A\u0932 \u0906\u0909\u091F\u092A\u0941\u091F."
lastmod: '2024-04-05T21:53:54.285791-06:00'
model: gpt-4-1106-preview
summary: "(\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) \u0938\u0948\u0902\u092A\
  \u0932 \u0906\u0909\u091F\u092A\u0941\u091F."
title: "\u0926\u094B \u0924\u093E\u0930\u0940\u0916\u094B\u0902 \u0915\u0940 \u0924\
  \u0941\u0932\u0928\u093E"
weight: 27
---

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
