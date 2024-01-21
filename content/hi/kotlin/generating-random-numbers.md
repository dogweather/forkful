---
title:                "यादृच्छिक संख्याएँ उत्पन्न करना"
date:                  2024-01-20T17:49:58.295875-07:00
model:                 gpt-4-1106-preview
simple_title:         "यादृच्छिक संख्याएँ उत्पन्न करना"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
रैंडम नंबर जेनरेटिंग यानी अनिश्चित संख्या उत्पन्न करना होता है। इसका उपयोग डेवलपर्स गेमिंग, सिमुलेशन, सुरक्षा, और टेस्टिंग में करते हैं जहाँ अप्रत्याशितता महत्वपूर्ण है।

## How To: (कैसे करें?)
```Kotlin
import kotlin.random.Random

fun main() {
    // एक रैंडम संख्या जेनरेट करें
    val randomNumber = Random.nextInt(0, 100)
    println("Random Number: $randomNumber") // उदाहरण: Random Number: 52

    // रैंडम डबल जेनरेट करें
    val randomDouble = Random.nextDouble(1.0, 10.0)
    println("Random Double: ${"%.2f".format(randomDouble)}") // उदाहरण: Random Double: 3.14
}
```

## Deep Dive (गहराई में जानकारी)
रैंडम नंबर जेनरेटर्स (RNGs) का इतिहास 1940 के दशक से है, जब शुरूआती कंप्यूटर्स के लिए वे विकसित किए गए थे। Kotlin में `Random` क्लास RNGs के साथ काम करने के लिए एक आसान API प्रदान करती है।

`java.util.Random` पुराना है और अब `kotlin.random.Random` से बदल दिया गया है, जो बेहतर प्रदर्शन और बेहतर API प्रदान करता है। कोटलिन के रैंडम नंबर जेनरेटर अंतर्गत रूप से सिएड वैल्यूज पे निर्भर करते हैं। इससे प्रत्येक रन पर अलग नतीजा मिलता है। 

RNGs की विश्वसनीयता और एंट्रापी कोल्होसन की गुणवत्ता पर बहुत निर्भर करती हैं। यहां, कोटलिन निश्चित नहीं होता - ये ओएस या हार्डवेयर RNG पर डिपेन्ड करता है।

## See Also (और भी जानकारी)
- Kotlin Random API documentation: [Kotlinlang.org API Reference](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.random/)
- Wikipedia on Random Number Generation: [Wikipedia RNG](https://en.wikipedia.org/wiki/Random_number_generation)
- Understanding Random Number Generation: [Computerphile RNG Explanation Video](https://www.youtube.com/watch?v=SxP30euw3-0)