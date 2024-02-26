---
date: 2024-01-20 17:48:20.872136-07:00
description: "\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0915\u0940 \u0932\
  \u0902\u092C\u093E\u0908 \u091C\u093E\u0928\u0928\u093E \u092E\u0924\u0932\u092C\
  \ \u0939\u0948 \u092A\u0924\u093E \u0915\u0930\u0928\u093E \u0915\u093F \u0938\u094D\
  \u091F\u094D\u0930\u093F\u0902\u0917 \u092E\u0947\u0902 \u0915\u093F\u0924\u0928\
  \u0947 \u0915\u0948\u0930\u0947\u0915\u094D\u091F\u0930\u094D\u0938 (characters)\
  \ \u0939\u0948\u0902\u0964 \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\
  \u0930\u094D\u0938 \u0907\u0938\u0947 \u0907\u0938\u0932\u093F\u090F \u0915\u0930\
  \u0924\u0947 \u0939\u0948\u0902 \u0915\u093F \u092F\u0939 \u091F\u0947\u0915\u094D\
  \u0938\u094D\u091F \u092A\u094D\u0930\u094B\u0938\u0947\u0938\u093F\u0902\u0917\
  ,\u2026"
lastmod: '2024-02-25T18:49:50.102015-07:00'
model: gpt-4-1106-preview
summary: "\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0915\u0940 \u0932\u0902\
  \u092C\u093E\u0908 \u091C\u093E\u0928\u0928\u093E \u092E\u0924\u0932\u092C \u0939\
  \u0948 \u092A\u0924\u093E \u0915\u0930\u0928\u093E \u0915\u093F \u0938\u094D\u091F\
  \u094D\u0930\u093F\u0902\u0917 \u092E\u0947\u0902 \u0915\u093F\u0924\u0928\u0947\
  \ \u0915\u0948\u0930\u0947\u0915\u094D\u091F\u0930\u094D\u0938 (characters) \u0939\
  \u0948\u0902\u0964 \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930\u094D\
  \u0938 \u0907\u0938\u0947 \u0907\u0938\u0932\u093F\u090F \u0915\u0930\u0924\u0947\
  \ \u0939\u0948\u0902 \u0915\u093F \u092F\u0939 \u091F\u0947\u0915\u094D\u0938\u094D\
  \u091F \u092A\u094D\u0930\u094B\u0938\u0947\u0938\u093F\u0902\u0917,\u2026"
title: "\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0915\u0940 \u0932\u0902\
  \u092C\u093E\u0908 \u091C\u094D\u091E\u093E\u0924 \u0915\u0930\u0928\u093E"
---

{{< edit_this_page >}}

## क्या और क्यों? (What & Why?)

स्ट्रिंग की लंबाई जानना मतलब है पता करना कि स्ट्रिंग में कितने कैरेक्टर्स (characters) हैं। प्रोग्रामर्स इसे इसलिए करते हैं कि यह टेक्स्ट प्रोसेसिंग, वैलिडेशन और UI डिज़ाइनिंग में अहम है।

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
