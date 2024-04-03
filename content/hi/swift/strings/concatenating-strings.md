---
date: 2024-01-20 17:36:25.560326-07:00
description: "How to (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902): Swift \u092E\
  \u0947\u0902 \u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917\u094D\u0938 \u091C\
  \u094B\u0921\u093C\u0924\u0947 \u0935\u0915\u093C\u094D\u0924 \u0906\u092A `+` \u0913\
  \u092A\u0930\u0947\u091F\u0930 \u092F\u093E `\\()` \u0938\u093F\u0902\u091F\u0947\
  \u0915\u094D\u0938 \u0915\u093E \u0907\u0938\u094D\u0924\u0947\u092E\u093E\u0932\
  \ \u0915\u0930 \u0938\u0915\u0924\u0947 \u0939\u0948\u0902\u0964 \u0909\u0926\u093E\
  \u0939\u0930\u0923 \u0926\u0947\u0916\u093F\u090F."
lastmod: '2024-03-13T22:44:52.904153-06:00'
model: gpt-4-1106-preview
summary: "Swift \u092E\u0947\u0902 \u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917\
  \u094D\u0938 \u091C\u094B\u0921\u093C\u0924\u0947 \u0935\u0915\u093C\u094D\u0924\
  \ \u0906\u092A `+` \u0913\u092A\u0930\u0947\u091F\u0930 \u092F\u093E `\\()` \u0938\
  \u093F\u0902\u091F\u0947\u0915\u094D\u0938 \u0915\u093E \u0907\u0938\u094D\u0924\
  \u0947\u092E\u093E\u0932 \u0915\u0930 \u0938\u0915\u0924\u0947 \u0939\u0948\u0902\
  \u0964 \u0909\u0926\u093E\u0939\u0930\u0923 \u0926\u0947\u0916\u093F\u090F."
title: "\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0915\u094B \u091C\u094B\
  \u0921\u093C\u0928\u093E"
weight: 3
---

## How to (कैसे करें):
Swift में स्ट्रिंग्स जोड़ते वक़्त आप `+` ओपरेटर या `\()` सिंटेक्स का इस्तेमाल कर सकते हैं। उदाहरण देखिए:

```Swift
// स्ट्रिंग्स को '+' ओपरेटर से जोड़ना
let greeting = "नमस्ते, "
let name = "विवेक!"
let welcomeMessage = greeting + name
print(welcomeMessage) // आउटपुट: नमस्ते, विवेक!

// String interpolation का इस्तेमाल करते हुए
let age = 30
let introduction = "मेरा नाम \(name) है और मेरी उम्र \(age) साल है।"
print(introduction) // आउटपुट: मेरा नाम विवेक! है और मेरी उम्र 30 साल है।
```

## Deep Dive (गहन जानकारी):
पुराने दिनों में, स्ट्रिंग्स जोड़ना ज्यादा जटिल होता था और परफॉर्मेंस के मुद्दे होते थे। Swift के आ जाने के बाद, स्ट्रिंग हैंडलिंग सुधर गई और और भी आसान बन गई। 

स्ट्रिंग Concatenation के अलावा, आप `append()` का इस्तेमाल कर सकते हैं जो मौजूदा स्ट्रिंग में एक और स्ट्रिंग जोड़ देता है। पर, जब बड़े डेटा सेट के साथ काम करते हैं, तब `joined()` या `StringBuilder` जैसी क्लासेज ज्यादा परफॉर्मेंट होती हैं। स्ट्रिंग्स को जोड़ते समय मेमोरी आवंटन को ठीक से मैनेज करना जरूरी होता है।

## See Also (और जानें):
- [Swift Documentation on Strings and Characters](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Swift API Reference for String](https://developer.apple.com/documentation/swift/string)
- [Ray Wenderlich String Concatenation in Swift](https://www.raywenderlich.com/)
  
इन लिंक्स पर जाकर आप स्ट्रिंग्स के बारे में और गहराई से जान सकते हैं, उनके मेथड्स के बारे में पढ़ सकते हैं, और प्रैक्टिकल एक्जाम्पल के जरिए सीख सकते हैं।
