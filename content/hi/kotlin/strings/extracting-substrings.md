---
date: 2024-01-20 17:46:20.446209-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902? (How to:) ."
lastmod: '2024-04-05T21:53:54.251697-06:00'
model: gpt-4-1106-preview
summary: ''
title: "\u0938\u092C\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917\u094D\u0938 \u0928\
  \u093F\u0915\u093E\u0932\u0928\u093E"
weight: 6
---

## कैसे करें? (How to:)
```kotlin
fun main() {
    val text = "नमस्कार, Kotlin!"

    // पहले 5 characters निकालें
    val substring1 = text.substring(0, 5)
    println(substring1) // नमस्कार

    // शब्द 'Kotlin' निकालें
    val kotlinWordIndex = text.indexOf("Kotlin")
    val substring2 = text.substring(kotlinWordIndex)
    println(substring2) // Kotlin!
}
```

## गहराई में जानकारी (Deep Dive)
सबस्ट्रिंग्स की क्षमता शुरुआती प्रोग्रामिंग भाषाओं से ही रही है - यह स्ट्रिंग मेंपुलेशन का एक बुनियादी हिस्सा है। Kotlin में `substring` फंक्शन दो प्रकार से प्रयोग किये जा सकते हैं: इंडेक्स रेंज देकर या शुरू और अंतिम इंडेक्स देकर। जावा की `String` कक्षा से Kotlin में हुआ यह सुधार ज्यादा सुगम और लचीला है। अल्टरनेटिव्स में `take`, `drop`, `filter`, `removeRange` इत्यादि फंक्शन्स शामिल हैं जो कस्टम सबस्ट्रिंग्स बनाने की अनुमति देते हैं।

## अन्य संसाधन (See Also)
- [Kotlin Documentation - Basic Types: Strings](https://kotlinlang.org/docs/basic-types.html#strings)
