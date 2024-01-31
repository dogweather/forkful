---
title:                "परीक्षण लिखना"
date:                  2024-01-19
html_title:           "Arduino: परीक्षण लिखना"
simple_title:         "परीक्षण लिखना"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/writing-tests.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
टेस्ट लेखन(यानि writing tests) से मतलब सॉफ्टवेयर कोड की जांच करना है, ताकि कोई त्रुटि न रहे। प्रोग्रामर इसलिए टेस्ट लिखते हैं, ताकि उनका कोड ठीक से काम करे और बग्स से मुक्त हो।

## कैसे करें:
```kotlin
// एक सरल फंक्शन जो दो अंकों का योग करता है।
fun sum(a: Int, b: Int) = a + b

// टेस्ट फंक्शन जो 'sum' फ़ंक्शन की जांच करता है।
fun main() {
    val expected = 10
    val result = sum(7, 3)
    assert(result == expected) { "Test failed: $result is not equal to $expected" }
    println("Test passed: $result is equal to $expected")
}

// सामान्य आउटपुट:
// Test passed: 10 is equal to 10
```

## गहराई से जानकारी:
कोटलिन टेस्ट लेखन की शुरुआत JUnit लाइब्रेरी से हुई, जो जावा के लिए बनाई गई थी। आजकल, Kotest और MockK जैसे फ्रेमवर्क भी प्रचलन में हैं। ये टूल्स कोड के विभिन्न हिस्सों की कार्यक्षमता और मजबूती की जांच करके, संभावित समस्याओं को पहचानते हैं और सुधारते हैं। 

## संबंधित स्रोत:
- [Kotest GitHub Repository](https://github.com/kotest/kotest)
- [MockK GitHub Repository](https://github.com/mockk/mockk)
