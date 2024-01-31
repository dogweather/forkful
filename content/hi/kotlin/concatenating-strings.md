---
title:                "स्ट्रिंग को जोड़ना"
date:                  2024-01-20T17:35:54.012061-07:00
model:                 gpt-4-1106-preview
simple_title:         "स्ट्रिंग को जोड़ना"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
स्ट्रिंग्स को जोड़ना मतलब दो या दो से ज़्यादा स्ट्रिंग्स को एक साथ चिपकाना। प्रोग्रामर्स यह इसलिए करते हैं क्योंकि अक्सर शब्दों या वाक्यों को बनाने और परिस्थिति के अनुसार डेटा दिखाने में जरूरत पड़ती है।

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
