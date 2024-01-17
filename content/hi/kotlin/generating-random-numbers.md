---
title:                "तस्याम्कालित संख्याएं उत्पन्न करना"
html_title:           "Kotlin: तस्याम्कालित संख्याएं उत्पन्न करना"
simple_title:         "तस्याम्कालित संख्याएं उत्पन्न करना"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/generating-random-numbers.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
रैंडम नंबर जनरेशन यानि कि ये कैसे काम करता है, ये क्यों प्रोग्रामर्स के लिए महत्वपूर्ण है - इसे लोगों के लिए जो जनरेट किए गए नंबर की जरूरत होती है, और सुरक्षा या गेम डिजाइन में आवश्यकता हो सकती है।

## कैसे करें:
```kotlin
// 1 से 100 तक के बीच से एक रैंडम नंबर लेना
val randomNumber = (1..100).random()

// 0 से 10,000 तक के बीच से धनराशि लेना
val money = (0..10000).random()

// दो से छह्ल तक के बीच से कोई एक संख्या चुनना
val number = (2..6).random()
```

## गहराई में गम्भीरता:
कई स्थितियों में, रैंडम नंबर जनरेशन को अन्य तकनीकों के लिए उपयोग किया जाता है जैसे कि नेटवर्क सुरक्षा या स्पॉर्ट्स बेटिंग। कुछ अल्टर्नेटिव्स जैसे pseudorandom और cryptographically secure random नंबर भी हैं। कोटलिन को रैंडम नंबर जनरेशन के लिए यूनिक्स के ऑपरेटिंग सिस्टम की विशेषताओं और java.util.Random फंक्शन का सहायता लिया जाता है। 

## और भी देखें:
• [Kotlin docs on Random numbers] (https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.random/kotlin.random/-random) 
• [A beginner's guide to random number generation] (https://www.geeksforgeeks.org/generating-random-numbers-in-kotlin/) 
• [Random number generation in game development] (https://gamedevelopment.tutsplus.com/tutorials/random-number-generation-in-game-development--cms-30019)