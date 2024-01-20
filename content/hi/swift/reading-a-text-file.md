---
title:                "एक पाठ फ़ाइल पढ़ना"
html_title:           "Bash: एक पाठ फ़ाइल पढ़ना"
simple_title:         "एक पाठ फ़ाइल पढ़ना"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/reading-a-text-file.md"
---

{{< edit_this_page >}}

## क्या और क्यों? 

टेक्स्ट फ़ाइल पढ़ना मतलब किसी मौजूदा टेक्स्ट फ़ाइल से डेटा खोजना और उसे पढ़ना। प्रोग्रामर इसे मौजूदा डेटा को संशोधित करने, ऐनालाइस करने या उस पर काम करने के लिए करते हैं।

## कैसे करें:

यहाँ आपकी Swift में टेक्स्ट फ़ाइल पढ़ने की उदाहरण कोड है:

```Swift
import Foundation

if let path = Bundle.main.path(forResource: "example", ofType: "txt") {
    do {
        let text = try String(contentsOfFile: path, encoding: .utf8)
        print(text)
    } catch {
        print("Error: \(error)")
    }
}
```

इस कोड का उद्देश्य 'example.txt' नामक फ़ाइल को पढ़ने और उसकी सामग्री को प्रिंट करना है।

## गहरा अध्ययन:

स्विफ्ट में टेक्स्ट फ़ाइल पढ़ने का कोई सीधा तरीका नहीं है। हमें इसे Foundation फ़्रेमवर्क से import करके String वर्ग का उपयोग करना पड़ता है, जो विभिन्न फ़ाइल पठन मेथड उपलब्ध कराता है। 

वैकल्पिक रूप से, हम NSData और InputStream का भी उपयोग कर सकते हैं, पर इनका इस्तेमाल जटिल हो सकता है और ये विशेष परिस्थितियों के लिए ही होते हैं।

यदि आपकी फ़ाइल बहुत बड़ी है, तो इसे छोटे टुकड़ों में पढ़ने पर विचार करें, जिससे प्रोग्राम की प्रदर्शन क्षमता पर नहीं पड़ेगा।

## एवं देखें: 

[Swift Documentation: Working with Files and Directories](https://developer.apple.com/documentation/foundation/filemanager)

[Reading & writing to a text file in Swift](https://www.hackingwithswift.com/example-code/system/how-to-read-and-write-a-string-from-text-file)

[Handling text files in Swift](https://www.andyibanez.com/posts/handling-text-files-in-swift/)

उम्मीद है की आपको यह टूटोरियल स्विफ्ट में फ़ाइल पठन की मूल जानकारी प्रदान कर सकता है। प्रत्येक परिस्थिति के लिए योग्यताओं के बारे में अधिक जानने के लिए ऊपर दी गई अतिरिक्त स्रोतों की जांच करें।