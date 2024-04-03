---
date: 2024-01-20 17:48:20.872136-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: (How to:) Swift \u092E\
  \u0947\u0902 \u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0915\u0940 \u0932\
  \u0902\u092C\u093E\u0908 \u0928\u093F\u0915\u093E\u0932\u0928\u093E \u0938\u0930\
  \u0932 \u0939\u0948\u0964 \u0921\u0947\u092E\u094B \u0915\u0947 \u0924\u094C\u0930\
  \ \u092A\u0930 \u0926\u0947\u0916\u093F\u090F."
lastmod: '2024-03-13T22:44:52.902621-06:00'
model: gpt-4-1106-preview
summary: "Swift \u092E\u0947\u0902 \u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917\
  \ \u0915\u0940 \u0932\u0902\u092C\u093E\u0908 \u0928\u093F\u0915\u093E\u0932\u0928\
  \u093E \u0938\u0930\u0932 \u0939\u0948\u0964 \u0921\u0947\u092E\u094B \u0915\u0947\
  \ \u0924\u094C\u0930 \u092A\u0930 \u0926\u0947\u0916\u093F\u090F."
title: "\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0915\u0940 \u0932\u0902\
  \u092C\u093E\u0908 \u091C\u094D\u091E\u093E\u0924 \u0915\u0930\u0928\u093E"
weight: 7
---

## कैसे करें: (How to:)
Swift में स्ट्रिंग की लंबाई निकालना सरल है। डेमो के तौर पर देखिए:

```Swift
let greeting = "नमस्ते"
let length = greeting.count
print("स्ट्रिंग की लंबाई है: \(length)")
```

इस कोड का आउटपुट होगा:

```
स्ट्रिंग की लंबाई है: 6
```

## गहराई से जानकारी (Deep Dive)
जब Swift पहली बार आया था, NSString का उपयोग करके लंबाई पाई जाती थी, जो कि Objective-C का हिस्सा था। लेकिन Swift में, `.count` प्रॉपर्टी के जरिये हाई लेवल और सहज तरीके से स्ट्रिंग की लंबाई मिलती है। एक विकल्प `.characters.count` था पर अब वह अनावश्यक है।

Unicode के कारण, स्ट्रिंग का `.count` इस्तेमाल करना सही रहता है क्योंकि यह सभी यूनिकोड कैरेक्टर्स को सही से गिनता है जबकि बाइट्स या UTF-16 कोड से नहीं। Swift का स्ट्रिंग हैंडलिंग डिज़ाइन Unicode से पूरा बना है।

## और देखें (See Also)
- Swift Standard Library Reference: [String](https://developer.apple.com/documentation/swift/string)
- Unicode.org's Unicode Standard: [https://www.unicode.org/standard/standard.html](https://www.unicode.org/standard/standard.html)
- Swift Book - Strings and Characters: [https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
