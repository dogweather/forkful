---
title:                "पाठ खोजना और बदलना"
aliases:
- /hi/swift/searching-and-replacing-text/
date:                  2024-01-20T17:59:18.177714-07:00
model:                 gpt-4-1106-preview
simple_title:         "पाठ खोजना और बदलना"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

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
