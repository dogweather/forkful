---
title:    "Swift: स्ट्रिंग्स को संयुक्त करना"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## क्यों
कस्टॉम शब्दों को बनाने के लिए आपको अलग-अलग स्ट्रिंग को एक साथ जोड़ने की आवश्यकता होती है। इसके लिए हम स्ट्रिंग को जोड़ने की तकनीक का उपयोग करते हैं।

## कैसे करें
```Swift
let firstName = "जॉन"
let lastName = "डो"
let fullName = firstName + " " + lastName
print(fullName)
```
प्रिंट के लिए: जॉन डो

## गहराई तक जाएँ
कस्टम शब्दों को बनाने के लिए, हम Swift में स्ट्रिंग को जोड़ने के लिए '+' ऑपरेटर का उपयोग कर सकते हैं। स्ट्रिंग्स को जोड़ने के अन्य तरीके भी हैं, जैसे कि '.append फ़ंक्शन' या '+= ऑपरेटर'। इन तरीकों का उपयोग करके आप अपने कोड को और अधिक स्पष्ट और उचित बना सकते हैं।

## देखिये भी
- [Swift Strings](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [String Concatenation in Swift](https://www.hackingwithswift.com/example-code/strings/how-to-concatenate-strings-in-swift)