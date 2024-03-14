---
date: 2024-01-20 17:55:38.417647-07:00
description: "\u092A\u093E\u0920 \u092B\u093E\u0907\u0932 \u0915\u094B \u092A\u0922\
  \u093C\u0928\u093E \u092E\u0924\u0932\u092C \u0939\u0948 \u0915\u093F \u0939\u092E\
  \ \u092B\u093E\u0907\u0932 \u092E\u0947\u0902 \u0932\u093F\u0916\u0947 \u0939\u0941\
  \u090F \u091F\u0947\u0915\u094D\u0938\u094D\u091F \u0915\u094B \u0938\u093F\u0938\
  \u094D\u091F\u092E \u092E\u0947\u0902 \u0932\u093E \u0930\u0939\u0947 \u0939\u0948\
  \u0902\u0964 \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930\u094D\u0938\
  \ \u092F\u0939 \u0907\u0938\u0932\u093F\u090F \u0915\u0930\u0924\u0947 \u0939\u0948\
  \u0902 \u0915\u094D\u092F\u094B\u0902\u0915\u093F \u0905\u0915\u094D\u0938\u0930\
  \ \u0921\u0947\u091F\u093E \u092A\u094D\u0930\u094B\u0938\u0947\u0938\u093F\u0902\
  \u0917,\u2026"
lastmod: '2024-03-13T22:44:52.948284-06:00'
model: gpt-4-1106-preview
summary: "\u092A\u093E\u0920 \u092B\u093E\u0907\u0932 \u0915\u094B \u092A\u0922\u093C\
  \u0928\u093E \u092E\u0924\u0932\u092C \u0939\u0948 \u0915\u093F \u0939\u092E \u092B\
  \u093E\u0907\u0932 \u092E\u0947\u0902 \u0932\u093F\u0916\u0947 \u0939\u0941\u090F\
  \ \u091F\u0947\u0915\u094D\u0938\u094D\u091F \u0915\u094B \u0938\u093F\u0938\u094D\
  \u091F\u092E \u092E\u0947\u0902 \u0932\u093E \u0930\u0939\u0947 \u0939\u0948\u0902\
  \u0964 \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930\u094D\u0938\
  \ \u092F\u0939 \u0907\u0938\u0932\u093F\u090F \u0915\u0930\u0924\u0947 \u0939\u0948\
  \u0902 \u0915\u094D\u092F\u094B\u0902\u0915\u093F \u0905\u0915\u094D\u0938\u0930\
  \ \u0921\u0947\u091F\u093E \u092A\u094D\u0930\u094B\u0938\u0947\u0938\u093F\u0902\
  \u0917,\u2026"
title: "\u091F\u0947\u0915\u094D\u0938\u094D\u091F \u092B\u093C\u093E\u0907\u0932\
  \ \u092A\u0922\u093C\u0928\u093E"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
पाठ फाइल को पढ़ना मतलब है कि हम फाइल में लिखे हुए टेक्स्ट को सिस्टम में ला रहे हैं। प्रोग्रामर्स यह इसलिए करते हैं क्योंकि अक्सर डेटा प्रोसेसिंग, कॉन्फ़िगरेशन, या फाइलों से इनपुट पढ़ने की ज़रुरत पड़ती है।

## How to: (कैसे करें:)
Swift में टेक्स्ट फाइल पढ़ने के लिए नीचे दिए गए कोड का इस्तेमाल करें:

```Swift
import Foundation

func readTextFromFile(fileName: String) {
    if let path = Bundle.main.path(forResource: fileName, ofType: "txt") {
        do {
            let text = try String(contentsOfFile: path, encoding: .utf8)
            print(text)
        } catch {
            print("Error reading file.")
        }
    } else {
        print("File Not Found.")
    }
}

// फ़ंक्शन कॉल करें
readTextFromFile(fileName: "example")
```

अगर फाइल `example.txt` में "नमस्ते स्विफ्ट!" हो, तो आउटपुट होगा:
```
नमस्ते स्विफ्ट!
```

## Deep Dive (गहराई में जानकारी)
पहले, फाइलों को पढ़ने के लिए C जैसी भाषाओं में कम्प्लेक्स कोडिंग की जाती थी। Swift ने यह काम आसान बना दिया है। विकल्प के रूप में, `FileManager` या नेटवर्किंग के जरिए `URLSession` का इस्तेमाल भी किया जा सकता है। `String(contentsOfFile:)` सिंपल फाइलों के लिए आदर्श है, लेकिन बड़ी फाइलों के लिए `InputStream` बेहतर रहेगा।

## See Also (और जानकारी के लिए)
- Swift की बुक: [The Swift Programming Language](https://docs.swift.org/swift-book)
- फाइल हैंडलिंग और `FileManager` का उपयोग: [File System Programming Guide](https://developer.apple.com/library/archive/documentation/FileManagement/Conceptual/FileSystemProgrammingGuide/Introduction/Introduction.html)
