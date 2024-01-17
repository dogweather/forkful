---
title:                "स्ट्रिंग की लंबाई ढूंढना"
html_title:           "Kotlin: स्ट्रिंग की लंबाई ढूंढना"
simple_title:         "स्ट्रिंग की लंबाई ढूंढना"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
एक शब्द की लंबाई का पता लगाना एक आम कार्य है जो कि एक स्ट्रिंग प्रोग्रामिंग में होता है। इसे प्रोग्रामर इसलिए करते हैं कि यह उन्हें इस्तेमाल करने वाली स्ट्रिंग की लंबाई की जानकारी देता है जिससे वे अपना कोड सही से चला सकते हैं।

## कैसे करें:
```Kotlin
var str = "कोटलिन"
println("String की लंबाई है: ${str.length}")
```
आपको यहां अपनी स्ट्रिंग को `str` नाम से अपनी पसंद की कोई भी स्ट्रिंग दर्ज करनी होगी। स्ट्रिंग का लंबाई हासिल करने के लिए, आपको `.length` फंक्शन का इस्तेमाल करना होगा। इससे आपको स्ट्रिंग की लंबाई वापस मिलेगी।

```Kotlin
var str = "Programming in Kotlin is fun!"
println("String की लंबाई है: ${str.length}")
```
यह कोड आपको नीचे दिए गए आउटपुट को देगा:
```
String की लंबाई है: 27
```

## गहराई तक जाएँ:
(1) इतिहास में, हम स्ट्रिंग की लंबाई को पता लगाने के लिए इस्तेमाल किया जाने वाले तरीके के टूल्स के बारे में जान सकते हैं। (2) अन्य विकल्प में, आप अपने कोड में `.length` फंक्शन के साथ साथ `String.prototype.len` फंक्शन का भी इस्तेमाल कर सकते हैं। एकाधिक लेख स्रोतों में यह जानकारी आपको बहुत ही फायदेमंद साबित होगी।

## अन्य लेख:
आप `Kotlin` से जुड़े अन्य लेखों को भी पढ़ सकते हैं। 
- स्ट्रिंग की लंबाई [कोटलिन 1.3 में मिले नए फंक्शन](https://kotlinexpertise.com/kotlin-string-length-functions/)
- `Kotlin` [कोडिंग बेस्ट प्रेक्टिस](https://kotlinlang.org/docs/reference/coding-conventions.html)
- `String` के साथ जुड़े [अन्य मौलिक ऑपरेशन](https://medium.com/@patelvishalv/stringbasicoperations-6d618a0bed1b)