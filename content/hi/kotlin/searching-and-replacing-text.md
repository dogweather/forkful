---
title:                "पाठ की खोज और प्रतिस्थापन"
html_title:           "Bash: पाठ की खोज और प्रतिस्थापन"
simple_title:         "पाठ की खोज और प्रतिस्थापन"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
टेक्स्ट खोजना और बदलना (searching and replacing) एक आम कार्य होता है जिसमें किसी विशिष्ट शब्द या वाक्यांश की खोज की जाती है और इसे अन्य शब्द या वाक्यांश से बदल दिया जाता है। प्रोग्रामर्स इसे करते हैं क्योंकि यह कोड में त्रुटियों को ठीक करने और कोड की कुशलता को बढ़ाने में मदद करता है।

## कैसे करें:
यहां Kotlin में टेक्स्ट खोजने और इसे बदलने का एक उदाहरण है:

```Kotlin
fun main() {
    var text = "Hello World!"
    text = text.replace("World", "Kotlin")
    println(text)
}
```
ऊपरी कोड का आउटपुट होगा:

```
Hello Kotlin!
```

## गहराई में:
१. हिस्टोरिकल कॉंटेक्स्ट: टेक्स्ट खोजने और बदलने की प्रक्रिया प्रोग्रामिंग की शुरुआत से ही थी। इसने प्रोग्रामर्स को बड़ी मात्रा में डाटा का प्रबंधन करने में मदद की है।

२. विकल्प: अलग-अलग कामों के लिए अलग-अलग विधियां होती हैं। उदाहरण के लिए, `Regex` का उपयोग खोजने और बदलने के अधिक उन्नत प्रक्रियाओं के लिए किया जा सकता है।

३. कार्यान्वयन का विवरण: `replace` फ़ंक्शन Kotlin में एक बिल्ट-इन फ़ंक्शन है जिसे `String` क्लास में परिभाषित किया गया है। यह एक `target` और `replacement` ऑब्जेक्ट लेता है, और फ़ंक्शन का सबसे पहला उद्घाटन टार्गेट के बारे में खोजता है और इसे बदलने का प्रयास करता है।

## देखने के लिए:
1. [Kotlin डॉक्यूमेंटेशन - रिप्लेस फंक्शन](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/replace.html)
2. [StackOverflow: Kotlin में स्ट्रिंग को कैसे बदलें](https://stackoverflow.com/questions/52213017/how-do-i-replace-string-in-kotlin)