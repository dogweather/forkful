---
title:                "टेक्स्ट फ़ाइल पढ़ना"
aliases: - /hi/swift/reading-a-text-file.md
date:                  2024-01-20T17:55:38.417647-07:00
model:                 gpt-4-1106-preview
simple_title:         "टेक्स्ट फ़ाइल पढ़ना"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/reading-a-text-file.md"
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
