---
date: 2024-01-20 17:41:40.221134-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) \u0938\u092E\
  \u092F \u0915\u0947 \u0938\u093E\u0925, \u0905\u0938\u094D\u0925\u093E\u092F\u0940\
  \ \u092B\u093E\u0907\u0932\u094B\u0902 \u0915\u0940 \u0906\u0935\u0936\u094D\u092F\
  \u0915\u0924\u093E \u092C\u0922\u093C\u0940 \u0939\u0948\u0964 \u092A\u0939\u0932\
  \u0947, \u0938\u093F\u0938\u094D\u091F\u092E\u094D\u0938 \u091F\u0947\u0902\u092A\
  \ \u0921\u093E\u092F\u0930\u0947\u0915\u094D\u091F\u0930\u0940\u091C\u093C \u0928\
  \u0939\u0940\u0902 \u0939\u094B\u0924\u0940 \u0925\u0940\u0902 \u092F\u093E \u092C\
  \u0939\u0941\u0924 \u092C\u0941\u0928\u093F\u092F\u093E\u0926\u0940 \u0939\u094B\
  \u0924\u0940 \u0925\u0940\u0902\u0964 \u0905\u092C,\u2026"
lastmod: '2024-04-05T22:51:07.612288-06:00'
model: gpt-4-1106-preview
summary: "(\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) \u0938\u092E\u092F\
  \ \u0915\u0947 \u0938\u093E\u0925, \u0905\u0938\u094D\u0925\u093E\u092F\u0940 \u092B\
  \u093E\u0907\u0932\u094B\u0902 \u0915\u0940 \u0906\u0935\u0936\u094D\u092F\u0915\
  \u0924\u093E \u092C\u0922\u093C\u0940 \u0939\u0948\u0964 \u092A\u0939\u0932\u0947\
  , \u0938\u093F\u0938\u094D\u091F\u092E\u094D\u0938 \u091F\u0947\u0902\u092A \u0921\
  \u093E\u092F\u0930\u0947\u0915\u094D\u091F\u0930\u0940\u091C\u093C \u0928\u0939\u0940\
  \u0902 \u0939\u094B\u0924\u0940 \u0925\u0940\u0902 \u092F\u093E \u092C\u0939\u0941\
  \u0924 \u092C\u0941\u0928\u093F\u092F\u093E\u0926\u0940 \u0939\u094B\u0924\u0940\
  \ \u0925\u0940\u0902\u0964 \u0905\u092C, \u092A\u094D\u0932\u0948\u091F\u092B\u0949\
  \u0930\u094D\u092E\u094D\u0938 \u091C\u0948\u0938\u0947 \u0915\u093F iOS \u0914\u0930\
  \ macOS \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930\u094D\u0938\
  \ \u0915\u094B NSTemporaryDirectory() \u091C\u0948\u0938\u0947 \u092E\u0947\u0925\
  \u0921\u094D\u0938 \u0926\u0947\u0924\u0947 \u0939\u0948\u0902\u0964 \u0905\u0932\
  \u094D\u091F\u0930\u0928\u0947\u091F\u093F\u0935\u094D\u0938 \u092E\u0947\u0902\
  \ \u0930\u0948\u092E-\u0921\u093F\u0938\u094D\u0915, in-memory \u0938\u094D\u091F\
  \u094B\u0930\u0947\u091C, \u092F\u093E custom-cacheing \u0938\u094B\u0932\u094D\u092F\
  \u0942\u0936\u0902\u0938 \u0939\u0948\u0902\u0964 Implement \u0915\u0930\u0928\u0947\
  \ \u092E\u0947\u0902, \u092B\u093E\u0907\u0932 \u0928\u093E\u092E \u0915\u0940 uniqueness\
  \ \u092E\u0939\u0924\u094D\u0935\u092A\u0942\u0930\u094D\u0923 \u0939\u0948 \u2013\
  \ UUIDs \u092F\u093E timestamps \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\
  \u0930\u0947\u0902\u0964 \u092F\u093E\u0926 \u0930\u0916\u0947\u0902, temporary\
  \ files \u0915\u094B \u0938\u093F\u0938\u094D\u091F\u092E \u0915\u0940 cleanup process\
  \ \u092E\u0947\u0902 \u0905\u092A\u0928\u0947 \u0906\u092A \u0939\u091F\u093E \u0926\
  \u093F\u092F\u093E \u091C\u093E \u0938\u0915\u0924\u093E \u0939\u0948\u0964."
title: "\u0905\u0938\u094D\u0925\u093E\u092F\u0940 \u092B\u093E\u0907\u0932 \u092C\
  \u0928\u093E\u0928\u093E"
weight: 21
---

## How to: (कैसे करें:)
```Swift
import Foundation

// अस्थायी डायरेक्टरी प्राप्त करना
let tempDirectory = NSTemporaryDirectory()

// अद्वितीय फाइल नाम जनरेट करना
let fileName = UUID().uuidString
let tempFilePath = (tempDirectory as NSString).appendingPathComponent(fileName)

// फाइल में डेटा लिखना
let sampleText = "यह एक अस्थायी फाइल है।"
do {
    try sampleText.write(toFile: tempFilePath, atomically: true, encoding: .utf8)
    print("फाइल सफलतापूर्वक बनाई गई: \(tempFilePath)")
} catch {
    print("फाइल बनाने में त्रुटि: \(error.localizedDescription)")
}

```

## Deep Dive (गहन अध्ययन):
समय के साथ, अस्थायी फाइलों की आवश्यकता बढ़ी है। पहले, सिस्टम्स टेंप डायरेक्टरीज़ नहीं होती थीं या बहुत बुनियादी होती थीं। अब, प्लैटफॉर्म्स जैसे कि iOS और macOS प्रोग्रामर्स को NSTemporaryDirectory() जैसे मेथड्स देते हैं।

अल्टरनेटिव्स में रैम-डिस्क, in-memory स्टोरेज, या custom-cacheing सोल्यूशंस हैं। Implement करने में, फाइल नाम की uniqueness महत्वपूर्ण है – UUIDs या timestamps का उपयोग करें। याद रखें, temporary files को सिस्टम की cleanup process में अपने आप हटा दिया जा सकता है।

## See Also (यह भी देखें):
- [FileManager Class Reference](https://developer.apple.com/documentation/foundation/filemanager)
- [UUID Class Reference](https://developer.apple.com/documentation/foundation/uuid)
