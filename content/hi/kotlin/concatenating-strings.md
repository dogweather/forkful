---
title:                "स्ट्रिंग्स को संयुक्त करना"
html_title:           "Kotlin: स्ट्रिंग्स को संयुक्त करना"
simple_title:         "स्ट्रिंग्स को संयुक्त करना"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/concatenating-strings.md"
---

{{< edit_this_page >}}

## क्यों

कभी-कभी हमारे पास अलग-अलग भाषाओं में लिखे गए टेक्स्ट होते हैं और हमें उनको एक साथ जोड़ना होता है। इसके लिए हम स्ट्रिंग को bond या concatenate करते हैं।

## कैसे करें

आपको अपने Kotlin प्रोग्राम में दो स्ट्रिंग को + ऑपरेटर का उपयोग करके जोड़ना है। यह डेमो कोड में दिखाया गया है:

```Kotlin
val hello = "नमस्ते "
val name = "जॉन"
val greeting = hello + name
println(greeting)
```

आउटपुट:

```Kotlin
नमस्ते जॉन
```

## गहराई में जाएं

स्ट्रिंग को जोड़ने के लिए Kotlin में + ऑपरेटर का उपयोग किया जाता है। इसके अलावा, आपको अन्य विकल्प भी मिलते हैं। एक विकल्प है string templates जो पदों को स्थानांतरित करता है, जैसे:

```Kotlin
val country = "भारत"
val message = "मैं $country में अपना दौरा पूरा करना चाहता हूं।"
println(message)
```

आउटपुट:

```Kotlin
मैं भारत में अपना दौरा पूरा करना चाहता हूं। 
```

इसके अलावा, आप स्ट्रिंग संयोजन के लिए ```concat()``` फ़ंक्शन का भी उपयोग कर सकते हो। यह एक युग्मन बनाता है जो दो स्ट्रिंग को जोड़ता है और एक नया स्ट्रिंग लौटाता है। उदाहरण:

```Kotlin
val str1 = "This is "
val str2 = "sample text"
val result = str1.concat(str2)
println(result)
```

आउटपुट:

```Kotlin
This is sample text
```

## देखें भी

- [Kotlin Strings](https://kotlinlang.org/docs/reference/basic-types.html#strings)
- [Kotlin String Concatenation](https://www.geeksforgeeks.org/kotlin-string-interpolation/)
- [Kotlin String Templates](https://www.baeldung.com/kotlin-string-templates)