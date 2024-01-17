---
title:                "टेक्स्ट को खोजें और बदलें"
html_title:           "Kotlin: टेक्स्ट को खोजें और बदलें"
simple_title:         "टेक्स्ट को खोजें और बदलें"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

"## क्या और क्यों?"

टेक्स्ट को सर्च और रिप्लेस करना अपने-आप पटले पार स्ट्रिंग्स को बदलना है। यह उपयोगकर्ता तालिका में बहुत ही उपयोगी है और वे यहां उपयोग करते हैं ताकि वे स्टाइल और खुद को अद्यतन कर सकें।

"## कैसे करें?"

```Kotlin
val str = "कौन सा खेल आपको अच्छा लगता है?"
val replaceStr = str.replace("अच्छा", "मजेदार")
println(replaceStr)
```

आउटपुट: 
कौन सा खेल आपको मजेदार लगता है?

"## गहराई में जाएं"

इतिहासिक पृष्ठभूमि के लिए, सर्च और रिप्लेस टेक्स्ट के बदलावों को पूरी तरह से अनुपरिवर्तित कर सकते हैं। इसके अलावा, कई अन्य विकल्प भी हैं जैसे कि रिप्लेसमैंट है। और टेक्स्ट मामलों के साथ बहु कोरोना कोडिंग उपयोग करता है जो आपको टेक्स्ट खोज और रिप्लेस करने में मदद कर सकता है।

"## इन्हें भी देखें"

- [कॉटलिन विभाग समूह](https://kotlinlang.org/docs/strings.html#string-replacement)
- [टेक्स्ट को खोजें और रिप्लेस करें वास्तव में आसान है कहां](https://blog.stylingandroid.com/string-search-replace-easier-than-you-think/)
- [कोटिन में स्ट्रिंग सर्च और रिप्लेस करने के 9 तरीके](https://www.tomdupont.net/2019/kotlin-string-search-replace-9-ways/)