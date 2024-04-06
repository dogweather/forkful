---
date: 2024-01-20 17:35:54.012061-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902) ."
lastmod: '2024-04-05T21:53:54.255676-06:00'
model: gpt-4-1106-preview
summary: ''
title: "\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0915\u094B \u091C\u094B\
  \u0921\u093C\u0928\u093E"
weight: 3
---

## How to: (कैसे करें)
```kotlin
fun main() {
    val greeting = "नमस्ते"
    val audience = "दुनिया"
    
    // '+' ऑपरेटर से जोड़ना
    val message = greeting + ", " + audience + "!"
    println(message) // नमस्ते, दुनिया!
    
    // String templates का इस्तेमाल करना
    val anotherMessage = "$greeting, $audience!"
    println(anotherMessage) // नमस्ते, दुनिया!
    
    // StringBuilder का इस्तेमाल करना
    val sb = StringBuilder()
    sb.append(greeting).append(", ").append(audience).append("!")
    println(sb.toString()) // नमस्ते, दुनिया!
}
```

## Deep Dive (गहराई से जानकारी)
पुराने समय में, स्ट्रिंग्स को जोड़ना कंप्यूटर के लिए महंगा था क्योंकि हर जोड़ के साथ नई मेमोरी बनानी पड़ती थी। Kotlin की शुरुआत में ही String templates और StringBuilder की सुविधा आ गई, जिनसे स्ट्रिंग को जोड़ने की प्रक्रिया सरल और कुशल बन गई। `+` ऑपरेटर जल्दी और सीधा तरीका है, लेकिन बड़े डेटा के लिए या लूप्स में StringBuilder का इस्तेमाल बेहतर माना जाता है जिससे परफॉरमेंस बेहतर रहती है।

## See Also (और भी देखें)
- Kotlin स्ट्रिंग्स के बारे में और जानने के लिए: [Kotlin Documentation on Strings](https://kotlinlang.org/docs/reference/basic-types.html#strings)

इस लेख के साथ, आप न सिर्फ स्ट्रिंग्स को जोड़ने की बुनियादी तकनीकों को सीख पाएंगे, बल्कि स्ट्रिंग्स के इस्तेमाल को अधिक कुशल बनाने के तरीकों को भी जान पाएंगे।
