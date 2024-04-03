---
date: 2024-01-20 17:37:58.061310-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: Swift \u092E\u0947\
  \u0902 `DateFormatter` \u0915\u093E \u0907\u0938\u094D\u0924\u0947\u092E\u093E\u0932\
  \ \u0915\u0930\u0924\u0947 \u0939\u0941\u090F \u0921\u0947\u091F \u0915\u094B \u0938\
  \u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u092E\u0947\u0902 \u092C\u0926\u0932\
  \ \u0938\u0915\u0924\u0947 \u0939\u0948\u0902\u0964 \u0928\u0940\u091A\u0947 \u0915\
  \u094B\u0921 \u0926\u093F\u092F\u093E \u0917\u092F\u093E \u0939\u0948."
lastmod: '2024-03-13T22:44:52.938060-06:00'
model: gpt-4-1106-preview
summary: "Swift \u092E\u0947\u0902 `DateFormatter` \u0915\u093E \u0907\u0938\u094D\
  \u0924\u0947\u092E\u093E\u0932 \u0915\u0930\u0924\u0947 \u0939\u0941\u090F \u0921\
  \u0947\u091F \u0915\u094B \u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u092E\
  \u0947\u0902 \u092C\u0926\u0932 \u0938\u0915\u0924\u0947 \u0939\u0948\u0902\u0964\
  \ \u0928\u0940\u091A\u0947 \u0915\u094B\u0921 \u0926\u093F\u092F\u093E \u0917\u092F\
  \u093E \u0939\u0948."
title: "\u0924\u093E\u0930\u0940\u0916 \u0915\u094B \u0938\u094D\u091F\u094D\u0930\
  \u093F\u0902\u0917 \u092E\u0947\u0902 \u092C\u0926\u0932\u0928\u093E"
weight: 28
---

## कैसे करें:
Swift में `DateFormatter` का इस्तेमाल करते हुए डेट को स्ट्रिंग में बदल सकते हैं। नीचे कोड दिया गया है:

```Swift
import Foundation

let currentDate = Date()

let formatter = DateFormatter()
formatter.dateFormat = "dd/MM/yyyy"

let dateString = formatter.string(from: currentDate)
print(dateString)
```

अगर आज 12 अप्रैल 2023 है, आउटपुट होगा: 
```
12/04/2023
```

## गहराई में जानकारी:
`DateFormatter` में `dateFormat` का इस्तेमाल करके डेट को विभिन्न प्रकार के स्ट्रिंग फॉर्मेट्स में प्रदर्शित किया जा सकता है। `DateFormatter` कौन्सेप्ट 1980s से है, जब आईबीएम के सिस्टम/३८ में समान उपकरण थे। वैकल्पिक रूप से, `ISO8601DateFormatter` का उपयोग करके ISO-8601 मानक तारीखें बनाई जा सकती हैं। Swift में `Date` और `String` के बीच परिवर्तन कई तरीकों से किया जा सकता है, जिसमें `.timeIntervalSince1970` या कस्टम पैर्टन्स जैसे `"EEEE, MMM d, yyyy"` भी शामिल हैं।

## संबंधित सूत्रों की जानकारी:
- [Apple's Date Formatting Guide](https://developer.apple.com/documentation/foundation/dateformatter)
- [Swift Documentation: DateFormatter](https://developer.apple.com/documentation/foundation/dateformatter)
