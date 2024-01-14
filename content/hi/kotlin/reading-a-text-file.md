---
title:                "Kotlin: एक टेक्स्ट फ़ाइल पढ़ना"
programming_language: "Kotlin"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/reading-a-text-file.md"
---

{{< edit_this_page >}}

## क्यों
एक टेक्स्ट फाइल को पढ़ना बहुत महत्वपूर्ण है क्योंकि यह सॉफ्टवेयर डेवलपमेंट और डेटा प्रोसेसिंग में आवश्यक है। इससे आप अपने सॉफ्टवेयर को डाटा कोडिंग के साथ बेहतर बना सकते हैं और यह भी देख सकते हैं कि आपका कोड सही से व्यवस्थित है या नहीं।

## कैसे करें
टेक्स्ट फाइल पढ़ने के लिए कोट्लिन का उपयोग करने के लिए, आपको `FileReader` को इनपुट स्ट्रीम के साथ इनिशियलाइज करना होगा। उसके बाद, आप `readLines()` फंक्शन का उपयोग करके स्ट्रीम से लाईन एरे बना सकते हैं। अंत में, आप इस लाईन एरे के सदस्यों को `forEach` लूप में छाप सकते हैं।

```Kotlin
val reader = FileReader("file.txt")
val lines = reader.readLines()

lines.forEach { println(it) }
```

इसका उत्पादन निम्नांकित प्रकार हो सकता है:

```commandline
Hello
नमस्ते
Hola
こんにちは
```

## गहराई में जाएं
टेक्स्ट फाइल को पढ़ने के अतिरिक्त, कोटलिन में अन्य भी कई तरीके मौजूद हैं। आप `BufferedReader` का उपयोग करके स्ट्रीम से डेटा को पढ़ सकते हैं और आप `Scanner` का उपयोग करके इनपुट टोकन के साथ खेल सकते हैं। इसके साथ ही, आप `useLines()` फंक्शन का उपयोग करके भी स्ट्रीम से लाईनों को पढ़ सकते हैं और इसे प्रक्रिया को ज्यादा अन्यायरिक बना सकते हैं।

## देखें भी
- [Kotlin File I/O Documentation](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/kotlin.-file/java.io.-file/read-lines.html)
- [Kotlin Tutorial on Reading a File](https://kotlinlang.org/docs/tutorials/kotlin-for-py/reading-files.html)
- [Kotlin File I/O Example](https://jurlabs.com/blog/reading-a-file-with-kotlin)