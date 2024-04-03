---
date: 2024-01-26 04:44:17.359932-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: \u0906\u0907\u090F\
  \ Kotlin \u092E\u0947\u0902 \u090F\u0915 \u092E\u0942\u0932 \u091C\u091F\u093F\u0932\
  \ \u0938\u0902\u0916\u094D\u092F\u093E \u0915\u094D\u0932\u093E\u0938 \u092A\u0930\
  \u093F\u092D\u093E\u0937\u093F\u0924 \u0915\u0930\u0947\u0902."
lastmod: '2024-03-13T22:44:52.248271-06:00'
model: gpt-4-0125-preview
summary: "\u0906\u0907\u090F Kotlin \u092E\u0947\u0902 \u090F\u0915 \u092E\u0942\u0932\
  \ \u091C\u091F\u093F\u0932 \u0938\u0902\u0916\u094D\u092F\u093E \u0915\u094D\u0932\
  \u093E\u0938 \u092A\u0930\u093F\u092D\u093E\u0937\u093F\u0924 \u0915\u0930\u0947\
  \u0902."
title: "\u091C\u091F\u093F\u0932 \u0938\u0902\u0916\u094D\u092F\u093E\u0913\u0902\
  \ \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\u092E \u0915\u0930\u0928\u093E"
weight: 14
---

## कैसे करें:
आइए Kotlin में एक मूल जटिल संख्या क्लास परिभाषित करें:

```kotlin
data class Complex(val real: Double, val imaginary: Double) {
    operator fun plus(other: Complex) = Complex(real + other.real, imaginary + other.imaginary)
    operator fun minus(other: Complex) = Complex(real - other.real, imaginary - other.imaginary)
    operator fun times(other: Complex) = Complex(
        real * other.real - imaginary * other.imaginary,
        real * other.imaginary + imaginary * other.real
    )
    
    override fun toString(): String = "($real + ${imaginary}i)"
}

fun main() {
    val a = Complex(1.0, 2.0)
    val b = Complex(3.0, 4.0)
    
    println("a + b = ${a + b}")  // आउटपुट: a + b = (4.0 + 6.0i)
    println("a - b = ${a - b}")  // आउटपुट: a - b = (-2.0 - 2.0i)
    println("a * b = ${a * b}")  // आउटपुट: a * b = (-5.0 + 10.0i)
}
```

## गहराई से विश्लेषण
जटिल संख्याओं का पहली बार उल्लेख 16वीं सदी में हुआ था, ऐसे घन समीकरणों को हल करते हुए जिनमें वास्तविक समाधान नहीं थे। इंजीनियरिंग और भौतिकी विज्ञान जटिल संख्याओं से विशेष लाभ उठाते हैं AC सर्किट्स और तरंग रूपों का विश्लेषण करने के लिए। आप भारी-भरकम कार्य के लिए Kotlin की `koma` या `ejml` जैसी लाइब्रेरी का उपयोग वैकल्पिक रूप से कर सकते हैं।

जटिल संख्याओं पर किए गए कार्य वास्तविक संख्याओं के साथ मिलते-जुलते हैं, लेकिन काल्पनिक इकाई के प्रति ध्यान देने की आवश्यकता होती है। उदाहरण के लिए, गुणन वितरित
 संपत्ति का अनुसरण करता है, इस बात को याद रखते हुए कि `i^2 = -1`। यह काल्पनिक इकाई हमें बहु-आयामी संख्याओं को प्रदर्शित करने की क्षमता प्रदान करती है, विभिन्न वैज्ञानिक गणनाओं में महत्वपूर्ण होती है।

## देखें भी
Kotlin गणित लाइब्रेरी:

- [koma](https://koma.kyonifer.com/): Kotlin के लिए एक वैज्ञानिक कंप्यूटिंग लाइब्रेरी।

जटिल संख्याओं पर आगे पढ़ने के लिए:

- [Wikipedia: जटिल संख्या](https://en.wikipedia.org/wiki/Complex_number)
