---
title:                "एक टेक्स्ट फ़ाइल पढ़ना"
html_title:           "Swift: एक टेक्स्ट फ़ाइल पढ़ना"
simple_title:         "एक टेक्स्ट फ़ाइल पढ़ना"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/reading-a-text-file.md"
---

{{< edit_this_page >}}

# क्या और क्यों?
टेक्स्ट फ़ाइल को पढ़ना क्या है और क्यों प्रोग्रामर यह करते हैं - यह बताने के लिए दो तीन सेंटेंस होंगे।

## क्या है और क्यों करें?
टेक्स्ट फ़ाइल टेक्स्ट को होस्ट करती है जो सामान्य बाइनरी फ़ाइलों से भिन्न होती है। इसे प्रोग्रामर्स उनके कोड के साथ टेक्स्ट फ़ाइल से डेटा पढ़ने या सेव करने के लिए इस्तेमाल करते हैं।

## कैसे करें?
```Swift
let fileURL = Bundle.main.url(forResource: "textfile", withExtension: "txt") // फाइल का यूआरएल बनाएं
do {
  let content = try String(contentsOf: fileURL!, encoding: .utf8) // फाइल से सामग्री लें
  print(content) // सामग्री को प्रिंट करें
}
catch {
  print("Error reading file: \(error)") // अगर कोई त्रुटि सामग्री पढ़ते समय होती है, तो यह दिखाएँ।
}
```

## समावेश
यह परिचय स्तंभ के लिए अत्यधिक हो सकते हैं, लेकिन यह थोड़े से प्रोग्रामिंग जानकारों के लिए उपयोगी हो सकता है। आपके पास अन्य विकल्प भी हैं जो टेक्स्ट फ़ाइल देख सकते हैं, जैसे कि कमांड लाइन उपयोगकर्ता इंटरफ़ेस (CLI) या किसी अन्य आधार आधारित लाइब्रेरी।

## और भी देखें
टेक्स्ट फ़ाइल और समान विषयों के लिए यहाँ दिए गए स्रोतों को देखें:
- [How to Read a Text File in Swift (Hindi)](https://code4developers.com/how-to-read-a-text-file-in-swift/)
- [Command Line Interface Tutorial (Hindi)](https://www.includehelp.com/command-line-interface-swift/)
- [How to Read and Write Text Files in Swift](https://www.hackingwithswift.com/example-code/system/how-to-read-and-write-files-in-swift)