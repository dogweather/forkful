---
title:                "यादृच्छिक संख्याएँ उत्पन्न करना"
date:                  2024-01-20T17:50:08.146612-07:00
model:                 gpt-4-1106-preview
simple_title:         "यादृच्छिक संख्याएँ उत्पन्न करना"
programming_language: "Swift"
category:             "Swift"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
रैंडम नंबर्स उत्पन्न करने से अभिप्राय ऐसे नंबर्स तैयार करना होता है जो कोई पैटर्न नहीं दिखाते। प्रोग्रामर्स ऐसे नंबर्स गेमिंग, सिमुलेशन, सिक्योरिटी, और डेटा एनालिसिस में उपयोग करते हैं।

## How to: (कैसे करें:)
Swift में रैंडम नंबर्स पैदा करना आसान है। यहाँ कुछ उदाहरण हैं:

```Swift
// बुनियादी इंटीजर रैंडम नंबर
let randomInt = Int.random(in: 1...100)
print(randomInt)

// फ्लोटिंग पॉइंट रैंडम नंबर
let randomFloat = Float.random(in: 0..<1)
print(randomFloat)

// रैंडम बूलियन
let randomBool = Bool.random()
print(randomBool)
```

जब तक आपका कोड चलता है, आपको अलग-अलग नतीजे दिखेंगे।

## Deep Dive (गहराई में जानकारी)
स्विफ्ट की मौजूदा रैंडमनेस API नए हैं, पहले प्रोग्रामर्स को C लाइब्रेरीज जैसे कि `arc4random` या `/dev/random` उपयोग करने पड़ते थे। यह नई API सुरक्षित और क्रॉस-प्लेटफ़ॉर्म संगतता देती है। Alternatives में `GameplayKit` की `GKRandomSource` क्लास शामिल है, लेकिन उसका उपयोग मुख्य रूप से गेम डेवेलपमेंट के लिए किया जाता है। रैंडमनेस के नियमन के लिए, मैथ के Pseudo-random number generators (PRNG) का इस्तेमाल होता है जो deterministically रैंडम-जैसे नंबर्स बनाते हैं।

## See Also (और जानकारी के लिए)
- [Using GameplayKit to Generate Random Numbers in Swift](https://developer.apple.com/documentation/gameplaykit/gkrandomdistribution)
