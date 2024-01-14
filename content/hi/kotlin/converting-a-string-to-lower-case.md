---
title:                "Kotlin: स्ट्रिंग को लोअर केस में परिवर्तित करना"
simple_title:         "स्ट्रिंग को लोअर केस में परिवर्तित करना"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## क्यों

आज की डिजिटल दुनिया में, हम अपनी डेटा को संरक्षित रखना चाहते हैं। इसलिए, इसमें से एक है कि हम अपने स्ट्रिंग को लोअर केस में बदल सकें ताकि वह दूसरों को पढ़ने के लिए आसान हो।

## कैसे करें

```Kotlin
fun main() {
    val name = "John Smith"
    val lowerCaseName = name.toLowerCase()
    println(lowerCaseName)
}
```

आउटपुट: john smith

## गहराई में जाएं

एक स्ट्रिंग को लोअर केस में बदलने के लिए, हम आमतौर पर String क्लास के लिए उपलब्ध toLowerCase() फ़ंक्शन का उपयोग करते हैं। यह फ़ंक्शन एक नया String रिटर्न करती है जिसमें सभी अक्षर लोअर केस में होते हैं। इसके अलावा, हम अपने स्ट्रिंग को लोअर केस में बदलने के लिए, अन्य विकल्पों जैसे String.lowercase() और String.lowercase(Locale.getDefault()) का भी उपयोग कर सकते हैं।

## इससे जुड़े लिंक

- [Kotlin विकीपीडिया पृष्ठ](https://en.wikipedia.org/wiki/Kotlin_(programming_language)#Strings)
- [Kotlin toLowerCase() डॉक्यूमेंटेशन](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/to-lower-case.html)
- [दो तरीकों से कोटलिन स्ट्रिंग को लोअर केस में बदलें](https://www.baeldung.com/kotlin/string-lowercase)