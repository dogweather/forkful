---
title:                "एक तारीख को स्ट्रिंग में परिवर्तित करना"
html_title:           "Java: एक तारीख को स्ट्रिंग में परिवर्तित करना"
simple_title:         "एक तारीख को स्ट्रिंग में परिवर्तित करना"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों? (What & Why?)

व्यापार क्रियाओं, टाइम-स्टैम्प, एप्लिकेशन के लग और डेटा यात्रा के लिए, हमें आमतौर पर दिनांक को विभिन्न स्वरूपों में बदलने की आवश्यकता होती है। इसलिए कोडर्स को स्विफ्ट में दिनांक को स्ट्रिंग में बदलने का विधान जानना चाहिए।

## कैसे (How To)

यहां स्विफ्ट के DateFormatter का उपयोग करके दिनांक को स्ट्रींग में कनवर्ट करने का एक उदाहरण है:

```swift
import Foundation

let date = Date()
let formatter = DateFormatter()
formatter.dateFormat = "yyyy-MM-dd HH:mm:ss"
let result = formatter.string(from: date)

print(result)
```
जब आप इसे चलाते हैं, आपको वर्तमान दिनांक और समय यथा, "2022-05-07 15:40:50" प्राप्त होगा।

## गहरी जानकारी (Deep Dive)

DateFormatter वर्ग स्विफ्ट की Foundation फ्रेमवर्क का हिस्सा है और दिनांक और समय को मौखिक और लिखित रूप में प्राप्त करने या बदलने के लिए क्रियाओं को सज्जित करता है।

विकल्प रूप में, आप ISO8601DateFormatter वर्ग का उपयोग कर सकते हैं, यह एक और ISO 8601 दिनांक और समय के लिये स्वरूपक है।

डेटा को स्ट्रिंग में बदलने का विधान से उच्च प्रदर्शन का अनुमान नहीं किया जाना चाहिए क्योंकि यह भारी भरकम और महंगा हो सकता है। जहां संभव हो, इसे कम से कम उपयोग करना चाहिए।

## और भी देखें (See Also)

1. [Apple Documentation: DateFormatter](https://developer.apple.com/documentation/foundation/dateformatter)
2. [Apple Documentation: ISO8601DateFormatter](https://developer.apple.com/documentation/foundation/iso8601dateformatter)
3. [Stackoverflow: How to convert a Date into a string in Swift?](https://stackoverflow.com/questions/35700281/date-format-in-swift)