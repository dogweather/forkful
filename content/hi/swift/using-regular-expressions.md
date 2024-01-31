---
title:                "रेगुलर एक्सप्रेशन का उपयोग"
date:                  2024-01-19
html_title:           "Bash: रेगुलर एक्सप्रेशन का उपयोग"
simple_title:         "रेगुलर एक्सप्रेशन का उपयोग"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
रेगुलर एक्सप्रेशन्स (Regular expressions) पैटर्न मैचिंग का एक तरीका हैं, जो टेक्स्ट को सर्च और मैनिप्युलेट करने के लिए इस्तेमाल होते हैं। प्रोग्रामर्स इसका इस्तेमाल डाटा वेलिडेशन, पार्सिंग, और टेक्स्ट एनालिसिस के लिए करते हैं क्योंकि ये शक्तिशाली और फ्लेक्सिबल होते हैं।

## How to (कैसे करें):
```Swift
import Foundation

let input = "मेरा ईमेल user@example.com पर है।"
let pattern = "[A-Z0-9a-z._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}"
let regex = try! NSRegularExpression(pattern: pattern, options: [])

let matches = regex.matches(in: input, options: [], range: NSRange(location: 0, length: input.utf16.count))

if let match = matches.first {
    let range = Range(match.range, in: input)!
    let found = input[range]
    print("मिला ईमेल: \(found)")
}
```
आउटपुट:
`मिला ईमेल: user@example.com`

## Deep Dive (गहराई से जानकारी):
रेगुलर एक्सप्रेशन्स 1950 के दशक में थ्योरेटिक कंप्यूटर साइंस में आए थे। उनका मैचिंग पैटर्न्स शुरू में टेक्स्ट एडिटर्स और Unix टूल्स में इस्तेमाल हुआ। स्विफ्ट में, `NSRegularExpression` एपीआई का इस्तेमाल इनके लिए होता है। विकल्प के रूप में, `String` मेथड्स भी सरल पैटर्न्स को मैनेज कर सकते हैं। परफॉरमेंस के लिहाज से, जितना संभव हो सके पैटर्न सिम्पल रखना चाहिए।

## See Also (देखें भी):
- Swift की ऑफिशियल दस्तावेज़ पर NSRegularExpression: https://developer.apple.com/documentation/foundation/nsregularexpression
- रेगुलर एक्सप्रेशन्स के ट्यूटोरियल: https://www.regular-expressions.info/
- रेगुलर एक्सप्रेशन्स की किताबें और संसाधन: https://www.regexbuddy.com/regex.html
