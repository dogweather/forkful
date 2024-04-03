---
date: 2024-01-20 17:59:18.177714-07:00
description: "\u0938\u0930\u094D\u091A \u0914\u0930 \u0930\u093F\u092A\u094D\u0932\
  \u0947\u0938 \u091F\u0947\u0915\u094D\u0938\u094D\u091F \u0915\u093E \u092E\u0924\
  \u0932\u092C \u0939\u0948 \u0915\u093F\u0938\u0940 \u0938\u094D\u091F\u094D\u0930\
  \u093F\u0902\u0917 \u092E\u0947\u0902 \u0916\u093E\u0938 \u0936\u092C\u094D\u0926\
  \u094B\u0902 \u092F\u093E \u0935\u093E\u0915\u094D\u092F\u094B\u0902 \u0915\u094B\
  \ \u0922\u0942\u0901\u0922\u0915\u0930 \u0909\u0928\u094D\u0939\u0947\u0902 \u092C\
  \u0926\u0932\u0928\u093E\u0964 \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\
  \u0930\u094D\u0938 \u092F\u0947 \u0915\u0930\u0924\u0947 \u0939\u0948\u0902 \u0921\
  \u0947\u091F\u093E \u0938\u093E\u092B\u093C \u0915\u0930\u0928\u0947, \u092C\u0917\
  \ \u092B\u093F\u0915\u094D\u0938 \u0915\u0930\u0928\u0947,\u2026"
lastmod: '2024-03-13T22:44:52.892398-06:00'
model: gpt-4-1106-preview
summary: "\u0938\u0930\u094D\u091A \u0914\u0930 \u0930\u093F\u092A\u094D\u0932\u0947\
  \u0938 \u091F\u0947\u0915\u094D\u0938\u094D\u091F \u0915\u093E \u092E\u0924\u0932\
  \u092C \u0939\u0948 \u0915\u093F\u0938\u0940 \u0938\u094D\u091F\u094D\u0930\u093F\
  \u0902\u0917 \u092E\u0947\u0902 \u0916\u093E\u0938 \u0936\u092C\u094D\u0926\u094B\
  \u0902 \u092F\u093E \u0935\u093E\u0915\u094D\u092F\u094B\u0902 \u0915\u094B \u0922\
  \u0942\u0901\u0922\u0915\u0930 \u0909\u0928\u094D\u0939\u0947\u0902 \u092C\u0926\
  \u0932\u0928\u093E\u0964 \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930\
  \u094D\u0938 \u092F\u0947 \u0915\u0930\u0924\u0947 \u0939\u0948\u0902 \u0921\u0947\
  \u091F\u093E \u0938\u093E\u092B\u093C \u0915\u0930\u0928\u0947, \u092C\u0917 \u092B\
  \u093F\u0915\u094D\u0938 \u0915\u0930\u0928\u0947, \u0914\u0930 \u092F\u0942\u091C\
  \u0930 \u0907\u0928\u092A\u0941\u091F \u0915\u094B \u092E\u0948\u0928\u0947\u091C\
  \ \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F\u0964."
title: "\u092A\u093E\u0920 \u0916\u094B\u091C\u0928\u093E \u0914\u0930 \u092C\u0926\
  \u0932\u0928\u093E"
weight: 10
---

## What & Why? (क्या और क्यों?)
सर्च और रिप्लेस टेक्स्ट का मतलब है किसी स्ट्रिंग में खास शब्दों या वाक्यों को ढूँढकर उन्हें बदलना। प्रोग्रामर्स ये करते हैं डेटा साफ़ करने, बग फिक्स करने, और यूजर इनपुट को मैनेज करने के लिए।

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
