---
title:                "स्ट्रिंग की लंबाई ज्ञात करना"
aliases:
- hi/swift/finding-the-length-of-a-string.md
date:                  2024-01-20T17:48:20.872136-07:00
model:                 gpt-4-1106-preview
simple_title:         "स्ट्रिंग की लंबाई ज्ञात करना"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/finding-the-length-of-a-string.md"
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
