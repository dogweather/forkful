---
title:                "स्ट्रिंग को जोड़ना"
date:                  2024-01-20T17:36:25.560326-07:00
model:                 gpt-4-1106-preview
simple_title:         "स्ट्रिंग को जोड़ना"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
स्ट्रिंग्स का जोड़ Concatenating strings होता है जब हम दो या उससे ज्यादा टेक्स्ट पीसेज को एक साथ जोड़ते हैं। प्रोग्रामर्स यह इसलिए करते हैं ताकि वे वेरिएबल्स, यूज़र इनपुट, और स्टेटिक टेक्स्ट को एक सिंगल स्ट्रिंग में मर्ज कर सकें।

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
