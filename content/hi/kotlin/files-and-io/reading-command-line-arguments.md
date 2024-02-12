---
title:                "कमांड लाइन आर्गुमेंट्स पढ़ना"
aliases: - /hi/kotlin/reading-command-line-arguments.md
date:                  2024-01-20T17:57:17.447012-07:00
model:                 gpt-4-1106-preview
simple_title:         "कमांड लाइन आर्गुमेंट्स पढ़ना"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
कमांड लाइन आर्ग्यूमेंट पढ़ना यानी यूज़र से टर्मिनल के ज़रिए इनपुट लेना। प्रोग्रामर्स इसलिए करते हैं क्योंकि इससे सॉफ्टवेयर कस्टमाइज़ेशन आसान होता है और कोड को मल्टीपल सिचुएशन में चलाने की फ्लेक्सिबिलिटी मिलती है।

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
