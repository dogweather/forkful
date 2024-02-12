---
title:                "रेगुलर एक्सप्रेशन्स का उपयोग करना"
date:                  2024-02-03T19:18:56.672171-07:00
model:                 gpt-4-0125-preview
simple_title:         "रेगुलर एक्सप्रेशन्स का उपयोग करना"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/using-regular-expressions.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?

नियमित अभिव्यक्तियाँ (regex) पाठ प्रोसेसिंग के लिए एक शक्तिशाली टूल हैं, जो प्रोग्रामरों को उन्नत पैटर्न-मिलान तकनीकों के साथ स्ट्रिंग्स को खोजने, मिलान करने और मैनिपुलेट करने की अनुमति देती हैं। Kotlin में, regex का उपयोग करना वैलिडेशन, पार्सिंग या ट्रांसफ़ॉर्मेशन जैसे जटिल टेक्स्ट प्रोसेसिंग कार्यों को कुशलतापूर्वक प्रदर्शन करने में मदद करता है, जो साधारण स्ट्रिंग मैनिपुलेशन से लेकर जटिल टेक्स्ट विश्लेषण तक के कार्यों के लिए अनिवार्य है।

## कैसे करें:

### बेसिक मिलान
Kotlin में यदि आप देखना चाहते हैं कि कोई स्ट्रिंग विशेष पैटर्न से मैच करता है या नहीं, तो आप `Regex` क्लास के `matches` मेथड का उपयोग कर सकते हैं।

```kotlin
val pattern = "kotlin".toRegex()
val input = "I love kotlin"
val result = pattern.containsMatchIn(input)

println(result)  // आउटपुट: true
```

### स्ट्रिंग के भागों को ढूंढना और निकालना
यदि आप किसी स्ट्रिंग के उन भागों को ढूंढना चाहते हैं जो एक पैटर्न से मैच करते हैं, Kotlin आपको सभी मैचों पर इटरेट करने की अनुमति देता है:

```kotlin
val datePattern = "\\d{2}/\\d{2}/\\d{4}".toRegex()
val input = "Today's date is 07/09/2023."
val dates = datePattern.findAll(input)

for (date in dates) {
    println(date.value)
}
// आउटपुट: 07/09/2023
```

### पाठ को बदलना
किसी स्ट्रिंग के वह भाग जो एक पैटर्न से मैच करते हैं, उन्हें `replace` फ़ंक्शन का उपयोग करके सीधे बदलना सरल है:

```kotlin
val input = "Username: user123"
val sanitizedInput = input.replace("\\d+".toRegex(), "XXX")

println(sanitizedInput)  // आउटपुट: Username: userXXX
```

### स्ट्रिंग्स को विभाजित करना
एक स्ट्रिंग को सूची में विभाजित करें, जिसमें एक regex पैटर्न डिलीमीटर के रूप में उपयोग होता है:

```kotlin
val input = "1,2,3,4,5"
val numbers = input.split(",".toRegex())

println(numbers)  // आउटपुट: [1, 2, 3, 4, 5]
```

### तृतीय-पक्ष पुस्तकालय: Kotest
[Kotest](https://github.com/kotest/kotest) एक लोकप्रिय Kotlin परीक्षण पुस्तकालय है जो Kotlin के निर्मित रेगेक्स समर्थन का विस्तार करता है, विशेष रूप से परीक्षण मामलों में वैलिडेशन के लिए उपयोगी।

```kotlin
// मान लें कि Kotest आपके प्रोजेक्ट में जोड़ा गया है
import io.kotest.matchers.string.shouldMatch

val input = "kotlin@test.com"
input shouldMatch "\\S+@\\S+\\.com".toRegex()

// यदि इनपुट ईमेल पैटर्न से मैच करता है तो यह परीक्षा पास होगी।
```

अपने Kotlin अनुप्रयोगों में नियमित अभिव्यक्तियों को शामिल करके, आप कुशलतापूर्वक सोफिस्टिकेटेड टेक्स्ट प्रोसेसिंग का प्रदर्शन कर सकते हैं। चाहे आप उपयोगकर्ता इनपुट का वैलिडेशन कर रहे हों, डेटा निकाल रहे हों या स्ट्रिंग्स को ट्रांसफॉर्म कर रहे हों, रेगेक्स पैटर्न एक मजबूत समाधान प्रदान करते हैं।
