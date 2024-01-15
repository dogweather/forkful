---
title:                "पैटर्न को मिलाने वाले अक्षरों को हटाना"
html_title:           "Swift: पैटर्न को मिलाने वाले अक्षरों को हटाना"
simple_title:         "पैटर्न को मिलाने वाले अक्षरों को हटाना"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## क्यों

कोई भी किसी भी एक स्थिर तरीके से दिए गए पैटर्न को मैच करने वाले वर्णों को हटाने का उपयोग कर सकता हैं, जो कुछ विशेष स्ट्रिंग्स को प्रोसेस करने के लिए उपयोगी हो सकता है।

## कैसे करें

आप ```Swift String ```का उपयोग कर सकते हैं जो कि प्रत्येक विशेष वर्णक्रम को पीछे से और आगे से कैसे देखता है। निम्नलिखित कोड एक आसान उदाहरण देता है।

```Swift
let str = "abcdeffg"
let charToRemove: Character = "f"
let newStr = String(str.filter { $0 != charToRemove })

print(newStr) // output: "abcdegg"
```

## गहराई में जाएं

अधिक समझने के लिए, हम स्ट्रिंग को `substring` से `range` तक मैपिंग कर सकते हैं और तब उसे `replacingOccurrences` के साथ प्रोसेस कर सकते हैं। यह प्रक्रिया चरणों में विभिन्न तरीकों से पूरी की जा सकती है।

## देखें भी

- [Apple वेबसाइट पर एक्सप्लोर करें](https://developer.apple.com/documentation/swift/string/1643511-filter)
- [Swift के बारे में और अधिक जानें](https://swift.org/)