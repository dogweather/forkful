---
title:                "Kotlin: नियमित अभिव्यक्तियों का प्रयोग करना"
programming_language: "Kotlin"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/using-regular-expressions.md"
---

{{< edit_this_page >}}

## क्यों

यदि आप नियमित अभिव्यक्तियों का उपयोग करते हैं तो आपको अपने कोड को संकटमुक्त और साफ बनाने में मदद मिल सकती है। 

## कैसे करें

```
Kotlin
val regex = Regex("^[A-Za-z0-9+_.-]+@(.+)\$")

fun validateEmail(email: String): Boolean {
    return regex.matches(email)
}

validateEmail("example@email.com") // true
validateEmail("_invalid@address.com") // false
```

## गहराई से जानें

नियमित अभिव्यक्तियों का उपयोग करना जांचने से पहले, आपको परिभाषा करनी होगी जो आपकी जरूरतों को समायोजित करने में मदद करती है। आप भिन्न तरीकों से अभिव्यक्तियों का उपयोग कर सकते हैं, जैसे कि वार्जिंग, अवस्था और समूह। आप इन अभिव्यक्तियों को जोड़कर भी उन्हें और अधिक शक्तिशाली और उपयोगी बना सकते हैं। इस लेख में हम उन अभिव्यक्तियों का समीक्षण करेंगे जो Kotlin में उपलब्ध हैं। 

## देखें भी

- [Kotlin अभिव्यक्तियां क्या हैं](https://kotlinlang.org/docs/reference/keywords.html)
- [Kotlin दस्तावेजीकरण - नियमित अभिव्यक्तियां](https://kotlinlang.org/docs/reference/regular-expressions.html)
- [Kotlin नियमित अभिव्यक्तियों को उपयोग करना](https://www.programiz.com/kotlin-programming/regular-expression)