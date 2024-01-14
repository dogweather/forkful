---
title:    "Kotlin: स्ट्रिंग को लोअर केस में बदलना"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## क्यों

कोई भी कितना भी बड़ा पठनीय सामग्री और लिखा हुआ कोड हो, कभी-कभी हमारे द्वारा इस्तेमाल किया जाने वाले इनपुट डेटा की स्ट्रिंग स्वरूप से काफी साफ़ और संरचित होना चाहिए। इसलिए, कोड लिखने से पहले स्ट्रिंग को निचे करना (lower case) एक बेहतर विकल्प हो सकता है। 

## कैसे करें

```Kotlin
//यूजर को स्ट्रिंग दिया गया है
val string = "KOTLIN PROGRAMMING"

//स्ट्रिंग को लोअर केस में कनवर्ट करें
val lowerCaseString = string.toLowerCase()

//नतीजे को प्रिंट करें
println(lowerCaseString)

//आउटपुट: kotlin programming
```

## गहराई में जाएं

स्ट्रिंग को लोअर केस में कनवर्ट करने के लिए कोटलिन में `toLowerCase()` फंक्शन का इस्तेमाल किया जाता है। यह फंक्शन स्ट्रिंग के हर एक चरित्र को मूल्यांकन श्रृंखला (Unicode Character Set) में नियोजित किया जाता है। इससे, स्ट्रिंग में गलती से बड़े-बड़े अक्षरों का इस्तेमाल भी सही ढंग से हो जाता है। अतः, हमें स्ट्रिंग के साथ काम करने में आसानी होती है और कोड कंप्लिकेशन से बचाया जा सकता है।

## देखें भी

- [Kotlin string documentation](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/index.html)
- [Why is it important to follow coding conventions?](https://www.theserverside.com/feature/Why-is-it-important-to-follow-coding-conventions)
- [Kotlin Basics: Variables and Data Types](https://medium.com/@AnkitM007/kotlin-basics-variables-and-data-types-47eb4cdf7db0)