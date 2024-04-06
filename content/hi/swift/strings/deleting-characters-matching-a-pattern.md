---
date: 2024-01-20 17:43:48.022595-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) \u090A\u092A\
  \u0930 \u0926\u093F\u092F\u093E \u0917\u092F\u093E \u0915\u094B\u0921 `originalString`\
  \ \u0938\u0947 \u0938\u092D\u0940 non-word \u0914\u0930 non-space \u0915\u0948\u0930\
  \u0947\u0915\u094D\u091F\u0930\u094D\u0938 \u0915\u094B \u0939\u091F\u093E\u0924\
  \u093E \u0939\u0948, \u091C\u094B `pattern` \u092E\u0947\u0902 \u0926\u0930\u094D\
  \u0936\u093E\u092F\u093E \u0917\u092F\u093E \u0939\u0948\u0964."
lastmod: '2024-04-05T21:53:54.849243-06:00'
model: gpt-4-1106-preview
summary: "(\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) \u090A\u092A\u0930\
  \ \u0926\u093F\u092F\u093E \u0917\u092F\u093E \u0915\u094B\u0921 `originalString`\
  \ \u0938\u0947 \u0938\u092D\u0940 non-word \u0914\u0930 non-space \u0915\u0948\u0930\
  \u0947\u0915\u094D\u091F\u0930\u094D\u0938 \u0915\u094B \u0939\u091F\u093E\u0924\
  \u093E \u0939\u0948, \u091C\u094B `pattern` \u092E\u0947\u0902 \u0926\u0930\u094D\
  \u0936\u093E\u092F\u093E \u0917\u092F\u093E \u0939\u0948\u0964."
title: "\u092A\u0948\u091F\u0930\u094D\u0928 \u0938\u0947 \u092E\u0947\u0932 \u0916\
  \u093E\u0924\u0947 \u0905\u0915\u094D\u0937\u0930\u094B\u0902 \u0915\u094B \u0939\
  \u091F\u093E\u0928\u093E"
weight: 5
---

## How to: (कैसे करें:)
```Swift
import Foundation

let originalString = "यह एक परीक्षण स्ट्रिंग है! 123!"
let pattern = "[^\\w\\s]"

if let regex = try? NSRegularExpression(pattern: pattern, options: []) {
    let range = NSRange(location: 0, length: originalString.utf16.count)
    let modifiedString = regex.stringByReplacingMatches(in: originalString, options: [], range: range, withTemplate: "")
    print(modifiedString) // Output: यह एक परीक्षण स्ट्रिंग है 123
}
```

ऊपर दिया गया कोड `originalString` से सभी non-word और non-space कैरेक्टर्स को हटाता है, जो `pattern` में दर्शाया गया है।

## Deep Dive (विस्तार से जानकारी)
जब से स्ट्रिंग्स को हैंडल करने के लिए रेगुलर एक्सप्रेशंस का आविष्कार हुआ, कैरेक्टर्स को मैचिंग पैटर्न्स के जरिए हटाना बहुत आसान हो गया। `NSRegularExpression` Swift में यह कार्य बखूबी करता है। वैकल्पिक तौर पर, डेवलपर्स `filter` या `replacingOccurrences(of:with:)` जैसे फंक्शन्स का भी उपयोग कर सकते हैं, लेकिन ये इतने पावरफुल नहीं हैं जबकि पैटर्न्स कॉम्प्लेक्स हों।

## See Also (और भी जानकारी के लिए)
- [Apple Documentation on NSRegularExpression](https://developer.apple.com/documentation/foundation/nsregularexpression)
- [Ray Wenderlich Swift Regex Tutorial](https://www.raywenderlich.com/5765-regular-expressions-tutorial-getting-started-with-regex-in-swift)
