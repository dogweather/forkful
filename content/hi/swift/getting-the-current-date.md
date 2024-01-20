---
title:                "वर्तमान तारीख प्राप्त करना"
html_title:           "C#: वर्तमान तारीख प्राप्त करना"
simple_title:         "वर्तमान तारीख प्राप्त करना"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/getting-the-current-date.md"
---

{{< edit_this_page >}}

## क्या और क्यों? (What & Why?)

'मौजूदा दिनांक' समझना मतलब आज की तारीख जानना होता है। प्रोग्रामर इसे तब करते हैं जब उन्हें इनपुट, लॉग, या डेटाबेस के रिकॉर्ड के साथ समय की स्थिति को ट्रैक करना होता है।  

## कैसे करें: (How to)

उदाहरण के लिए, Swift की लाइब्रेरी 'Foundation' में Date क्लास का उपयोग कर सकते है:

```Swift
import Foundation

let date = Date()
print("Current date: \(date)")
```

यह कोड आपको मौजूदा तिथि और समय दिखाएगा।

## गहरी माहिती (Deep Dive)

Swift प्रोग्रामिंग भाषा का प्रयोग करने वाले डेवलपर्स 'Foundation' प्रकाश्य में उपलब्ध 'Date' तथा 'DateFormatter' कक्षाओं का इस्तेमाल कर सकते हैं। यह कक्षाएं ऑब्जेक्टीव-सी के नीतियों और ढांचे का पालन करने के लिए Cocoa तथा Cocoa Touch फ्रेमवर्क में परिभाषित की गई हैं।

```Swift
import Foundation

let date = Date()
let formatter = DateFormatter()

formatter.dateStyle = .long
let formattedDate = formatter.string(from: date)

print("Formatted date: \(formattedDate)")
```

विकल्प के रूप में, आप NSDate कक्षा का भी उपयोग कर सकते हैं, लेकिन यह बेहतरीन अभ्यास नहीं माना जाता है क्योंकि यह वेतनमानी है। 

## और भी देखें (See Also)

[Official Apple Documentation](https://developer.apple.com/documentation/foundation/date)

[Swift Programming Guide](https://docs.swift.org/swift-book/LanguageGuide/TheBasics.html)

ये दस्तावेज़ और गाइड आपको Swift में तारीख और समय के साथ काम करने में मदद करेंगे।