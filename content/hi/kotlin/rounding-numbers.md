---
title:                "संख्याओं को पूर्णांक बनाना"
date:                  2024-01-26T03:47:19.608534-07:00
model:                 gpt-4-0125-preview
simple_title:         "संख्याओं को पूर्णांक बनाना"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/rounding-numbers.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

संख्याओं को गोल करना का मतलब है उन्हें निकटतम पूर्ण संख्या में समायोजित करना या एक निर्दिष्ट सटीकता की डिग्री तक। प्रोग्रामर इसे पठनीयता में सुधार के लिए, भंडारण आवश्यकताओं को कम करने के लिए, या क्योंकि प्रत्यक्ष मूल्य उत्तरकालीन गणनाओं के लिए महत्वपूर्ण नहीं है, करते हैं।

## कैसे:

Kotlin में, `roundToInt()`, `roundToDouble()` और अधिक नियंत्रण के लिए `BigDecimal` का उपयोग करके संख्याओं को गोल किया जा सकता है:

```kotlin
fun main() {
    val number1 = 3.14159
    println(number1.roundToInt()) // आउटपुट: 3

    val number2 = 3.5
    println(number2.roundToInt()) // आउटपुट: 4

    val number3 = 123.456
    println("%.2f".format(number3)) // आउटपुट: 123.46
    
    val bigDecimal = number3.toBigDecimal().setScale(1, RoundingMode.HALF_EVEN)
    println(bigDecimal) // आउटपुट: 123.5
}
```

## गहन समीक्षा

ऐतिहासिक रूप से, संख्याओं को गोल करना गणित और गणना दोनों में एक मौलिक अवधारणा रही है, जिसे संख्यात्मक सटीकता की सीमाओं से निपटने के लिए डिजाइन किया गया था। प्रारंभिक कंप्यूटिंग में, स्मृति की उच्च लागत के कारण गोलाई महत्वपूर्ण थी।

Kotlin में, गोलाई मानक Java पुस्तकालयों पर निर्मित है। गोलाई के विकल्पों में `Math.round()` शामिल है, जो निकटतम पूर्ण संख्या में गोल करता है, और `BigDecimal` कस्टम गोलाई के लिए शामिल है, जहाँ आप एक पैमाना और `RoundingMode` निर्दिष्ट कर सकते हैं।

प्रत्येक `RoundingMode` में संबंध (जब अंक गोलाई के विकल्पों के बीच बिलकुल बीच में होता है) के लिए विभिन्न नीतियां होती हैं। उदाहरण के लिए, `RoundingMode.HALF_UP` निकटतम पड़ोसी में गोल करता है, जब तक दोनों पड़ोसी समान दूरी पर न हों, ऐसे मामले में यह ऊपर की ओर गोल करता है।

## यह भी देखें

- Kotlin पर डॉक्यूमेंटेशन [`BigDecimal`](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/java.math.-big-decimal/index.html)
- Oracle की Java डॉक्यूमेंटेशन के लिए [`RoundingMode`](https://docs.oracle.com/javase/8/docs/api/java/math/RoundingMode.html)
- फ्लोटिंग-पॉइंट अरिथमेटिक के लिए IEEE मानक (IEEE 754) [IEEE मानक 754](https://ieeexplore.ieee.org/document/4610935)