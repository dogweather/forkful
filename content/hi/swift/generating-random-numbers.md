---
title:    "Swift: यादृच्छिक संख्याओं का उत्पादन"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/hi/swift/generating-random-numbers.md"
---

{{< edit_this_page >}}

## क्यों

यदि आप कभी सेही सेही आंकड़े या संख्याएं चाहते हों जिसमे स्पर्मिकता हो तो स्विफ्ट में यादृच्छिक संख्या जनरेशन आपको विशेष फायदे दे सकता है।

## कैसे करें

स्विफ्ट में यादृच्छिक संख्या जनरेशन के लिए दो मुख्य तरीके हैं - `Int.random(in:lowerBound...upperBound)` और `Double.random(in:lowerBound...upperBound)`। नीचे दिए गए कोड ब्लॉक में आप दोनों तरीकों का प्रयोग देख सकते हैं:

```Swift
// Int आंकड़े 1 से 10 के बीच में यादृच्छिक रूप से उत्पन्न करें:
let randomInt = Int.random(in: 1...10)
print(randomInt)
// उत्पाद: हर बार अलग-अलग - 2, 10, 8, 1, आदि.

// Double आंकड़े 10.5 से 20.5 के बीच में यादृच्छिक रूप से उत्पन्न करें:
let randomDouble = Double.random(in: 10.5...20.5)
print(randomDouble)
// उत्पाद: हर बार अलग-अलग - 14.235, 19.897, 20.01, आदि.
```

## गहराई में जाएं

यादृच्छिक संख्या जनरेशन वास्तव में एक बहुत ही महत्वपूर्ण गुण है। इससे आप आपके प्रोग्राम में स्पर्मिकता ला सकते हैं जो कि बहुत ही उपयोगी हो सकता है, क्योंकि हर बार अलग-अलग संख्याएं उत्पन्न होंगी। समय-समय पर, आप अपने प्रोग्राम में इस्तेमाल किए गए यादृच्छिक संख्याओं को रखना और पुनः उपयोग करना भी सीख सकते हैं।

## देखें भी

- [Random Number Generation in Swift](https://www.hackingwithswift.com/sixty/4/2/random-number-generation-in-swift)
- [Generating Random Numbers in Swift](https://medium.com/ios-os-x-development/generating-random-numbers-in-swift-actodabe8f7b)
- [Randomness in Swift](https://nshipster.com/random/)