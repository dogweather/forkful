---
title:                "सबस्ट्रिंग्स निकालना"
aliases:
- /hi/swift/extracting-substrings.md
date:                  2024-01-20T17:47:12.085096-07:00
model:                 gpt-4-1106-preview
simple_title:         "सबस्ट्रिंग्स निकालना"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
उपस्ट्रिंग निकालना मतलब है बड़ी स्ट्रिंग से छोटा हिस्सा बाहर खींचना। प्रोग्रामर्स ऐसा डेटा को प्रोसेस करने, जानकारी निकालने या स्ट्रिंग की मनचाही शेप देने के लिए करते हैं।

## How to: (कैसे करें:)
```Swift
let fullString = "Hello, Swift Programmers!"
let startIndex = fullString.index(fullString.startIndex, offsetBy: 7)
let endIndex = fullString.index(fullString.endIndex, offsetBy: -12)
let range = startIndex..<endIndex

// उपस्ट्रिंग निकालना
let substring = fullString[range]  // "Swift"

// उदाहरण आउटपुट
print(substring)  // "Swift"
```

## Deep Dive (गहराई से जानकारी):
जब हम स्ट्रिंग के पुराने तरीकों में देखते हैं, तो सबस्ट्रिंग्स को निकालने के लिए बहुत सिंपल API उपयोग होते थे, जैसे `substringWithRange:`। स्विफ्ट ने यह बदल दिया है और अब `String.Index` प्रकार का इस्तेमाल होता है, जो कि जटिल लग सकता है लेकिन यह स्ट्रिंग में यूनिकोड स्कैलर वैल्यूज को सही तरीके से हैंडल करता है। इससे मल्टीबाइट यूनिकोड करैक्टर्स के साथ काम करते समय एरर्स से बचा जा सकता है।

विकल्प के रूप में, रेगेक्स (regex) या अन्य बिल्ट-इन स्ट्रिंग फंक्शंस भी हैं जो स्पेसिफिक पैटर्न्स या डिलिमिटर्स के जरिए सबस्ट्रिंग्स निकालने में उपयोगी होते हैं।

## See Also (और जानकारी के लिए:)
- आधिकारिक स्विफ्ट डॉक्यूमेंटेशन: [String and Characters](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- Swift में स्ट्रिंग ऑपरेशंस: [Swift String Cheat Sheet](https://www.hackingwithswift.com/example-code/strings)
- यूनिकोड प्रोसेसिंग: [Swift’s String and Character API](https://oleb.net/blog/2017/11/swift-4-strings/)
