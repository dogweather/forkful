---
title:                "Swift: टेक्स्ट खोजना और बदलना"
programming_language: "Swift"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## क्यों

इस ब्लॉग पोस्ट में हम स्विफ्ट प्रोग्रामिंग के माध्यम से उब्दों और मूल्यों की जगह तलाशने और बदलने के बारे में बात करेंगे। यह उपयोगकर्ता के लिए बहुत ही हासिल कार्य हो सकता है।

## कैसे करें

स्विफ्ट में टेक्स्ट तलाशने और बदलने का अनुरोध ```String``` में इन्हें अपडेट करके किया जाता है। नीचे एक उदाहरण दिया गया है:

```Swift
var message = "मैं हिंदी में हूं।"
message = message.replacingOccurrences(of: "में", with: "से")
print(message)
```

आउटपुट: "मैं हिंदी से हूं।"

## गहराई में जाएं

अगर आपको किसी स्ट्रिंग के आंशिक उपस्थिति को बदलने की जरूरत है, तो आप regular expressions का उपयोग कर सकते हैं। ```NSRegularExpression``` के साथ आपको एक मजबूत खंडन इंजन मिलता है जो कुछ भी तलाशने और बदलने में मदद कर सकता है। नीचे एक उदाहरण दिया गया है:

```Swift
let input = "सर्वश्रेष्ठ भविष्य के लिए Swift है।"
let regex = try! NSRegularExpression(pattern: "भविष्य")
let output = regex.stringByReplacingMatches(in: input, range: NSRange(input.startIndex..., in: input), withTemplate: "भविष्य के लिए उत्तम")
print(output)
```

आउटपुट: "सर्वश्रेष्ठ भविष्य के लिए उत्तम Swift है।"

## देखें भी

- [Swift String पढ़ना और बदलना](https://developer.apple.com/documentation/swift/string)
- [NSRegularExpression के साथ काम करना](https://developer.apple.com/documentation/foundation/nsregularexpression)