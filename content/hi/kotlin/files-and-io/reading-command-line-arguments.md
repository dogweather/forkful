---
date: 2024-01-20 17:57:17.447012-07:00
description: "How to (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902): \u0905\u0917\
  \u0930 \u0906\u092A \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E \u0915\
  \u094B \u0915\u0941\u091B \u0907\u0938 \u0924\u0930\u0939 \u091A\u0932\u093E\u090F\
  \u0902."
lastmod: '2024-04-05T21:53:54.289400-06:00'
model: gpt-4-1106-preview
summary: "\u0905\u0917\u0930 \u0906\u092A \u092A\u094D\u0930\u094B\u0917\u094D\u0930\
  \u093E\u092E \u0915\u094B \u0915\u0941\u091B \u0907\u0938 \u0924\u0930\u0939 \u091A\
  \u0932\u093E\u090F\u0902."
title: "\u0915\u092E\u093E\u0902\u0921 \u0932\u093E\u0907\u0928 \u0906\u0930\u094D\
  \u0917\u0941\u092E\u0947\u0902\u091F\u094D\u0938 \u092A\u0922\u093C\u0928\u093E"
weight: 23
---

## How to (कैसे करें):
```kotlin
fun main(args: Array<String>) {
    if (args.isNotEmpty()) {
        println("नमस्ते, ${args[0]}!")
    } else {
        println("कमांड लाइन आर्ग्यूमेंट नहीं मिला।")
    }
}
```
अगर आप प्रोग्राम को कुछ इस तरह चलाएं:
```
$ kotlinc YourProgram.kt -include-runtime -d YourProgram.jar
$ java -jar YourProgram.jar आपका_नाम
```
आपको आउटपुट में दिखेगा:
```
नमस्ते, आपका_नाम!
```

## Deep Dive (गहराई में जानकारी):
कमांड लाइन आर्ग्यूमेंट पढ़ने का इस्तेमाल लगभग हर प्रोग्रामिंग भाषा में होता है। Kotlin में `main` फंक्शन के अंदर `args` एरे के ज़रिये ये डेटा आता है। अलटर्नेटिव्स जैसे कि पर्यावरण वेरियेबल्स या कॉन्फ़िगरेशन फाइल्स भी यूज़ किए जाते हैं। 

वैसे तो यह कॉन्सेप्ट सिंपल है, लेकिन बड़े प्रोग्राम्स में कमांड लाइन पार्सिंग लाइब्रेरीज़ की ज़रूरत हो सकती है जो ज़्यादा जटिल इनपुट्स को असानी से हैंडल कर सकें।

## See Also (देखें भी):
- Kotlin documentation on functions (फंक्शन्स पर Kotlin डॉक्युमेंटेशन): [Kotlin Functions](https://kotlinlang.org/docs/functions.html)
- Command-line Argument Parsing in Kotlin with kotlinx.cli (Kotlin में kotlinx.cli के साथ कमांड लाइन अर्गुमेंट पार्सिंग): [kotlinx.cli](https://github.com/Kotlin/kotlinx-cli)
- Comprehensive guide to command-line interfaces in Kotlin (Kotlin में कमांड लाइन इंटरफेसेस के लिए व्यापक मार्गदर्शिका): [CLI in Kotlin](https://ajalt.github.io/clikt/)
