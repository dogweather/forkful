---
title:                "स्ट्रिंग्स को जोड़ना"
html_title:           "Bash: स्ट्रिंग्स को जोड़ना"
simple_title:         "स्ट्रिंग्स को जोड़ना"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/concatenating-strings.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

स्ट्रिंग्स को कोनकेटनेट करने का मतलब होता है दो या दो से अधिक स्ट्रिंग्स को एक साथ जोड़ना। कोनकेटनेशन का उपयोग कोड को पढ़ने और समझने में आसानी के लिए स्ट्रिंग्स को उद्योगी तरीके से जोड़ने के लिए किया जाता है।

## कैसे करें:

Swift में, आप "+" ऑपरेटर का उपयोग करके स्ट्रिंग्स को कोनकेटनेट कर सकते हैं:

```Swift 
let firstString = "Hello, "
let secondString = "World!"
let fullString = firstString + secondString
print(fullString)
```

ऊपरी कोड का परिणाम "Hello, World!" प्रिंट होगा।

## गहराई में:

1. ऐतिहासिक प्रसंग: Swift की पहली वर्शन में स्ट्रिंग्स को कोनकेटनेट करने के लिए केवल "+" ऑपरेटर का उपयोग किया जा सकता था। हालांकि, Swift 5 ने इंटरपोलेशन की सुविधा प्रदान की, जिसमें स्ट्रिंग्स को कोनकेटनेट करने के लिए एक और तरीका जोड़ा गया।

2. विकल्प: स्ट्रिंग इंटरपोलेशन का उपयोग करके आप वेरिएबल्स और कंस्टेंट्स को सीधे स्ट्रिंग में जोड़ सकते हैं:

```Swift
let name = "Amit"
let greeting = "Namaste, \(name)"
print(greeting)
```

ऊपरी कोड का परिणाम "Namaste, Amit" प्रिंट होगा।

3. लागू करने का विवरण: स्ट्रिंग्स को कोनकेटनेट करते समय, हमें ध्यान में रखना चाहिए कि स्ट्रिंग्स इम्यूटेबल होती हैं। इसका मतलब है कि जब हम एक स्ट्रिंग को दूसरे स्ट्रिंग से कोनकेटनेट करते हैं, तो हम वास्तव में एक नई स्ट्रिंग बना रहे होते हैं, पुरानी स्ट्रिंग को बदलने के बजाय।

## देखने के लिए:

- [Apple's Swift Documentation](https://developer.apple.com/documentation/swift/string)
- [Swift String Interpolation](https://www.hackingwithswift.com/quick-start/understanding-swift/how-does-string-interpolation-work-in-swift)
- [Concatenating Strings and Characters](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)