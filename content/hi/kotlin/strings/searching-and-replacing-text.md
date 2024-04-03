---
date: 2024-01-20 17:58:23.500870-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) ."
lastmod: '2024-03-13T22:44:52.232700-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u092A\u093E\u0920 \u0916\u094B\u091C\u0928\u093E \u0914\u0930 \u092C\u0926\
  \u0932\u0928\u093E"
weight: 10
---

## How to: (कैसे करें:)
```Kotlin
fun main() {
    val originalText = "कोटलिन बहुत मजेदार है।"
    val searchText = "मजेदार"
    val replaceWith = "आसान"

    val newText = originalText.replace(searchText, replaceWith)
    println(newText) // परिणाम: कोटलिन बहुत आसान है।
}
```

उपरोक्त कोड में, `replace` फंक्शन `originalText` में `searchText` को `replaceWith` से बदलता है।

## Deep Dive (गहराई से जानकारी)
खोज और प्रतिस्थापन की सुविधा विभिन्न प्रोग्रामिंग भाषाओं में होती है। कोटलिन में यह बहुत सरल है, पर इसकी जड़ें शुरुआती टेक्स्ट एडिटिंग उपकरणों तक जाती हैं। विकल्पों में रेग्युलर एक्सप्रेशन (regex) का प्रयोग शामिल है, जो जटिल खोज पैटर्न के लिए उपयोगी है। कोटलिन में प्रतिस्थापन की क्रिया एक्सटेंशन फंक्शंस द्वारा संभव होती है, जो `String` क्लास की क्षमता बढ़ाते हैं। 

```Kotlin
fun main() {
    val regexText = "कोटलिन 12345 मजेदार है।"
    val regex = "\\d+".toRegex() // डिजिट का पैटर्न

    val replacedText = regex.replace(regexText, "हासिल")
    println(replacedText) // कोटलिन हासिल मजेदार है।
}
```

इस उदाहरण में, `\\d+` रेग्युलर एक्सप्रेशन किसी भी डिजिट के अनुक्रम को ढूँढता है, और `toRegex` उसे रेग्युलर एक्सप्रेशन में परिवर्तित करता है।

## See Also (और भी जानें)
- Kotlin `String` class reference: [Kotlin API](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/)
