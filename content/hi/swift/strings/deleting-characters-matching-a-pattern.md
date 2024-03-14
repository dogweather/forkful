---
date: 2024-01-20 17:43:48.022595-07:00
description: "\u092A\u0948\u091F\u0930\u094D\u0928 \u0915\u0947 \u0939\u093F\u0938\
  \u093E\u092C \u0938\u0947 \u0915\u0948\u0930\u0947\u0915\u094D\u091F\u0930\u094D\
  \u0938 \u0915\u094B \u0939\u091F\u093E\u0928\u093E \u092E\u0924\u0932\u092C \u0939\
  \u0948 \u0917\u093F\u0935\u0947\u0928 \u0936\u0930\u094D\u0924\u094B\u0902 \u0915\
  \u094B \u092E\u0948\u091A \u0915\u0930\u0928\u0947 \u0935\u093E\u0932\u0947 \u0905\
  \u0915\u094D\u0937\u0930\u094B\u0902 \u0915\u094B \u0938\u094D\u091F\u094D\u0930\
  \u093F\u0902\u0917 \u0938\u0947 \u0928\u093F\u0915\u093E\u0932\u0928\u093E\u0964\
  \ \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930 \u0910\u0938\u093E\
  \ \u0915\u094D\u092F\u094B\u0902 \u0915\u0930\u0924\u0947 \u0939\u0948\u0902? \u0938\
  \u093F\u092E\u094D\u092A\u0932 \u0939\u0948,\u2026"
lastmod: '2024-03-13T22:44:52.890674-06:00'
model: gpt-4-1106-preview
summary: "\u092A\u0948\u091F\u0930\u094D\u0928 \u0915\u0947 \u0939\u093F\u0938\u093E\
  \u092C \u0938\u0947 \u0915\u0948\u0930\u0947\u0915\u094D\u091F\u0930\u094D\u0938\
  \ \u0915\u094B \u0939\u091F\u093E\u0928\u093E \u092E\u0924\u0932\u092C \u0939\u0948\
  \ \u0917\u093F\u0935\u0947\u0928 \u0936\u0930\u094D\u0924\u094B\u0902 \u0915\u094B\
  \ \u092E\u0948\u091A \u0915\u0930\u0928\u0947 \u0935\u093E\u0932\u0947 \u0905\u0915\
  \u094D\u0937\u0930\u094B\u0902 \u0915\u094B \u0938\u094D\u091F\u094D\u0930\u093F\
  \u0902\u0917 \u0938\u0947 \u0928\u093F\u0915\u093E\u0932\u0928\u093E\u0964 \u092A\
  \u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930 \u0910\u0938\u093E \u0915\
  \u094D\u092F\u094B\u0902 \u0915\u0930\u0924\u0947 \u0939\u0948\u0902? \u0938\u093F\
  \u092E\u094D\u092A\u0932 \u0939\u0948,\u2026"
title: "\u092A\u0948\u091F\u0930\u094D\u0928 \u0938\u0947 \u092E\u0947\u0932 \u0916\
  \u093E\u0924\u0947 \u0905\u0915\u094D\u0937\u0930\u094B\u0902 \u0915\u094B \u0939\
  \u091F\u093E\u0928\u093E"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
पैटर्न के हिसाब से कैरेक्टर्स को हटाना मतलब है गिवेन शर्तों को मैच करने वाले अक्षरों को स्ट्रिंग से निकालना। प्रोग्रामर ऐसा क्यों करते हैं? सिम्पल है, कभी-कभी हमें पूरी तरह से साफ़ सुथरी स्ट्रिंग की जरूरत होती है, जैसे कि URL से स्पेशल कैरेक्टर्स हटाना या यूजर इनपुट से अनवांटेड स्पेसेज को ट्रिम करना।

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
