---
date: 2024-01-20 17:39:47.498129-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902? (How to:) Swift \u092E\
  \u0947\u0902 \u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0915\u094B \u0932\
  \u094B\u0905\u0930 \u0915\u0947\u0938 \u092E\u0947\u0902 \u0915\u0948\u0938\u0947\
  \ \u092C\u0926\u0932\u0947\u0902."
lastmod: '2024-04-05T21:53:54.852797-06:00'
model: gpt-4-1106-preview
summary: "(How to:) Swift \u092E\u0947\u0902 \u0938\u094D\u091F\u094D\u0930\u093F\u0902\
  \u0917 \u0915\u094B \u0932\u094B\u0905\u0930 \u0915\u0947\u0938 \u092E\u0947\u0902\
  \ \u0915\u0948\u0938\u0947 \u092C\u0926\u0932\u0947\u0902."
title: "\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0915\u094B \u091B\u094B\
  \u091F\u0947 \u0905\u0915\u094D\u0937\u0930\u094B\u0902 \u092E\u0947\u0902 \u092A\
  \u0930\u093F\u0935\u0930\u094D\u0924\u093F\u0924 \u0915\u0930\u0928\u093E"
weight: 4
---

## कैसे करें? (How to:)
Swift में स्ट्रिंग को लोअर केस में कैसे बदलें:

```Swift
let originalString = "नमस्ते World!"
let lowercasedString = originalString.lowercased()
print(lowercasedString)
// Output: नमस्ते world!
```

Sample दिखाता है कि कैसे अंग्रेज़ी के अक्षरों को छोटे में बदला जाता है और Hindi अक्षर पहले से ही lowercase में होते हैं।

## गहराई से जानकारी (Deep Dive)
लोअर केस कन्वर्ज़न साधारण लगता है, पर इसके पीछे काफी विचार और डिज़ाइन हैं। ASCII टेबल में, बड़े और छोटे अक्षरों के कोड अलग होते हैं। Unicode में, यह जटिल होता है क्योंकि एक से ज्यादा चिन्हों के लिए मामला होता है। Swift में `.lowercased()` मेथड इसे आसानी से हैंडल कर लेता है, पर यह भी लोकेल-आधारित हो सकता है (जैसे कि तुर्किश में "I" का lowercase "ı" होता है)।

अल्टरनेटिव तरीकों में, हम `lowercaseMap` या रेगुलर एक्सप्रेशन का उपयोग कर सकते हैं। लेकिन Swift का `.lowercased()` सिंपल और प्रदर्शन में अच्छा है। `.lowercased()` मेथड स्पेशल केसेस को भी हैंडल करता है और यह Unicode स्पेसिफिकेशन का पालन करता है।

## संबंधित लिंक (See Also)
- [String and Characters in Swift](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Unicode Standard](http://www.unicode.org/standard/standard.html)
