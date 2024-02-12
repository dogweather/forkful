---
title:                "स्ट्रिंग को छोटे अक्षरों में परिवर्तित करना"
aliases: - /hi/swift/converting-a-string-to-lower-case.md
date:                  2024-01-20T17:39:47.498129-07:00
model:                 gpt-4-1106-preview
simple_title:         "स्ट्रिंग को छोटे अक्षरों में परिवर्तित करना"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## क्या और क्यों? (What & Why?)
स्ट्रिंग को लोअर केस में बदलने का मतलब होता है सभी अक्षरों को छोटे (lowercase) में बदलना। प्रोग्रामर्स इसे तब करते हैं जब डेटा की तुलना करनी होती है या यूजर इनपुट को सॉर्ट और नॉर्मलाइज़ करना होता है। 

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
