---
date: 2024-01-20 17:59:18.177714-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) Swift \u092E\
  \u0947\u0902 \u091F\u0947\u0915\u094D\u0938\u094D\u091F \u0938\u0930\u094D\u091A\
  \ \u0914\u0930 \u0930\u093F\u092A\u094D\u0932\u0947\u0938 \u0938\u093F\u0902\u092A\
  \u0932 \u0939\u0948\u0964 `String` \u0915\u0947 \u092E\u0947\u0925\u0921\u094D\u0938\
  \ `replacingOccurrences(of:with:)` \u0914\u0930 `range(of:)` \u0907\u0938\u094D\u0924\
  \u0947\u092E\u093E\u0932 \u0915\u0930 \u0938\u0915\u0924\u0947 \u0939\u0948\u0902\
  \u0964\u2026"
lastmod: '2024-03-13T22:44:52.892398-06:00'
model: gpt-4-1106-preview
summary: "Swift \u092E\u0947\u0902 \u091F\u0947\u0915\u094D\u0938\u094D\u091F \u0938\
  \u0930\u094D\u091A \u0914\u0930 \u0930\u093F\u092A\u094D\u0932\u0947\u0938 \u0938\
  \u093F\u0902\u092A\u0932 \u0939\u0948\u0964 `String` \u0915\u0947 \u092E\u0947\u0925\
  \u0921\u094D\u0938 `replacingOccurrences(of:with:)` \u0914\u0930 `range(of:)` \u0907\
  \u0938\u094D\u0924\u0947\u092E\u093E\u0932 \u0915\u0930 \u0938\u0915\u0924\u0947\
  \ \u0939\u0948\u0902\u0964 \u092F\u0939\u093E\u0901 \u090F\u0915 \u0909\u0926\u093E\
  \u0939\u0930\u0923 \u0939\u0948."
title: "\u092A\u093E\u0920 \u0916\u094B\u091C\u0928\u093E \u0914\u0930 \u092C\u0926\
  \u0932\u0928\u093E"
weight: 10
---

## How to: (कैसे करें:)
Swift में टेक्स्ट सर्च और रिप्लेस सिंपल है। `String` के मेथड्स `replacingOccurrences(of:with:)` और `range(of:)` इस्तेमाल कर सकते हैं। यहाँ एक उदाहरण है:

```Swift
let originalString = "Swift मजेदार है। Swift सीखना आसान है।"
let searchString = "Swift"
let replacementString = "Programming"

// सर्च और रिप्लेस करने का तरीका
if originalString.contains(searchString) {
    let replacedString = originalString.replacingOccurrences(of: searchString, with: replacementString)
    print(replacedString) // "Programming मजेदार है। Programming सीखना आसान है।"
} else {
    print("सर्च स्ट्रिंग नहीं मिली।")
}
```

## Deep Dive (गहराई में जानकारी):
स्ट्रिंग में सर्च और रिप्लेस बहुत पहले से कंप्यूटर विज्ञान में है। इसके अलग-अलग तरीके और अलगोरिदम हैं, जैसे कि रेगुलर एक्सप्रेशन। Swift में `replacingOccurrences(of:with:)` डायरेक्ट और सिम्पल है, पर जटिल patterns के लिए, `NSRegularExpression` का इस्तेमाल कर सकते हैं। इम्प्लीमेंटेशन में, सर्च की एफिशिएंसी और रिप्लेस करते समय मेमोरी मैनेजमेंट ज़रूरी होता है।

## See Also (और भी जानें):
- Swift डॉक्युमेंटेशन `String` क्लास के लिए: [Swift String Documentation](https://developer.apple.com/documentation/swift/string)
- `NSRegularExpression` का उपयोग कैसे करें: [NSRegularExpression](https://developer.apple.com/documentation/foundation/nsregularexpression)
- रेगुलर एक्सप्रेशन्स की जानकारी के लिए: [Regular Expressions](https://www.regular-expressions.info/)
- एफिशिएंट स्ट्रिंग सर्चिंग अल्गोरिदम्स: [String Searching Algorithms](https://en.wikipedia.org/wiki/String-searching_algorithm)
