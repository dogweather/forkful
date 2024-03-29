---
date: 2024-01-20 17:35:54.012061-07:00
description: "\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917\u094D\u0938 \u0915\u094B\
  \ \u091C\u094B\u0921\u093C\u0928\u093E \u092E\u0924\u0932\u092C \u0926\u094B \u092F\
  \u093E \u0926\u094B \u0938\u0947 \u091C\u093C\u094D\u092F\u093E\u0926\u093E \u0938\
  \u094D\u091F\u094D\u0930\u093F\u0902\u0917\u094D\u0938 \u0915\u094B \u090F\u0915\
  \ \u0938\u093E\u0925 \u091A\u093F\u092A\u0915\u093E\u0928\u093E\u0964 \u092A\u094D\
  \u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930\u094D\u0938 \u092F\u0939 \u0907\
  \u0938\u0932\u093F\u090F \u0915\u0930\u0924\u0947 \u0939\u0948\u0902 \u0915\u094D\
  \u092F\u094B\u0902\u0915\u093F \u0905\u0915\u094D\u0938\u0930 \u0936\u092C\u094D\
  \u0926\u094B\u0902 \u092F\u093E \u0935\u093E\u0915\u094D\u092F\u094B\u0902 \u0915\
  \u094B \u092C\u0928\u093E\u0928\u0947 \u0914\u0930\u2026"
lastmod: '2024-03-13T22:44:52.244693-06:00'
model: gpt-4-1106-preview
summary: "\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917\u094D\u0938 \u0915\u094B\
  \ \u091C\u094B\u0921\u093C\u0928\u093E \u092E\u0924\u0932\u092C \u0926\u094B \u092F\
  \u093E \u0926\u094B \u0938\u0947 \u091C\u093C\u094D\u092F\u093E\u0926\u093E \u0938\
  \u094D\u091F\u094D\u0930\u093F\u0902\u0917\u094D\u0938 \u0915\u094B \u090F\u0915\
  \ \u0938\u093E\u0925 \u091A\u093F\u092A\u0915\u093E\u0928\u093E\u0964 \u092A\u094D\
  \u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930\u094D\u0938 \u092F\u0939 \u0907\
  \u0938\u0932\u093F\u090F \u0915\u0930\u0924\u0947 \u0939\u0948\u0902 \u0915\u094D\
  \u092F\u094B\u0902\u0915\u093F \u0905\u0915\u094D\u0938\u0930 \u0936\u092C\u094D\
  \u0926\u094B\u0902 \u092F\u093E \u0935\u093E\u0915\u094D\u092F\u094B\u0902 \u0915\
  \u094B \u092C\u0928\u093E\u0928\u0947 \u0914\u0930\u2026"
title: "\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0915\u094B \u091C\u094B\
  \u0921\u093C\u0928\u093E"
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
