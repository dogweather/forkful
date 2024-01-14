---
title:    "Kotlin: डिबग आउटपुट प्रिंट करना"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/printing-debug-output.md"
---

{{< edit_this_page >}}

## क्यों
कोटलिन में डिबग आउटपुट प्रिंट करने का उद्देश्य त्रुटियों को ढूंढने और उन्हें ठीक करने में मदद करता है।

## कैसे करें
```kotlin
fun main() {
    val name = "John"
    println("Hello $name!") // Output: Hello John!
}
```

```kotlin
fun main() {
    val num1 = 10
    val num2 = 5
    println("$num1 + $num2 = ${num1 + num2}") // Output: 10 + 5 = 15
}
```

```kotlin
fun main() {
    val list = listOf("apple", "banana", "orange")
    println("Fruits: $list") // Output: Fruits: [apple, banana, orange]
}
```

## गहराई में जाएं
डिबग आउटपुट के लिए ```println()``` फंक्शन का उपयोग किया जाता है। यह फंक्शन मैसेज के साथ वेरिएबल या एक्सप्रेशन को लेकर उसे प्रिंट करता है। इससे हम अपनी कोड में ट्रैक कर सकते हैं कि कैसे अपने प्रोग्राम में कोई गलती हो सकती है और उसे ठीक कैसे कर सकते हैं।

## देखें भी
[Kotlin एक पूर्वावलोकन](https://kotlinlang.org/docs/reference/) \
[Kotlin फंक्शन](https://kotlinlang.org/docs/reference/functions.html)