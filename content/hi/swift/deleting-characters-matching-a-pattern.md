---
title:                "पैटर्न से मिलते जुलते वर्णों को हटाना"
html_title:           "Elixir: पैटर्न से मिलते जुलते वर्णों को हटाना"
simple_title:         "पैटर्न से मिलते जुलते वर्णों को हटाना"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

पैटर्न से मेल खाते वर्णों को हटाना मतलब होता है कुछ निर्दिष्ट वर्णों को स्ट्रिंग से निकालना। प्रोग्रामर इसे अनावश्यक स्ट्रिंग के हिस्से हटाने, कस्टम फॉर्मैटिंग लागू करने और डेटा क्लीनअप करने के लिए करते हैं। 

## कैसे:

```Swift
let string = "नमस्ते दुनिया!"
let characterSet = CharacterSet(charactersIn: "नते दय!")
let result = string.trimmingCharacters(in: characterSet)
print(result) // prints "मस्तुनि"
```

इस कोड का उद्घाटन करें, आपको एक स्ट्रिंग `नमस्ते दुनिया!` मिलेगी। फिर `CharacterSet` `नते दय!` सेट किया जाता है। इसके बाद, हम स्ट्रिंग से किरदारों को हटाने के लिए `trimmingCharacters(in:)` का उपयोग करते हैं। अंत में, यदि हम परिणामत: प्रिंट करते हैं, तो "मस्तुनि" मिलेगा।

## गहरी चर्चा:

Swift में characters को हटाने के बारे में जब बात होती है, तो `trimmingCharacters(in:)` फंक्शन सबसे प्रयोगिक और मुख्य उपकरण होता है। यह फंक्शन Swift भाषा के पहले वर्जन से ही उपलब्ध है और इसकी वजह से, Swift प्रोग्रामर अपने स्ट्रिंग को आसानी से तैयार और संशोधित कर सकते हैं। इसके अतिरिक्त, आप `filter(_:)` फंक्शन का उपयोग करके कस्टम फ़िल्टर भी लागू कर सकते हैं। ऐसा करने पर आपको अधिक नियंत्रण मिलेगा, लेकिन यह अधिक कोड लिखने की आवश्यकता हो सकती है।

## देखें भी:

1. "How to Use Regex with Swift": [Link](https://www.raywenderlich.com/5765-regular-expressions-tutorial-getting-started)
2. "Swift String and Characters": [Link](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
3. "Understanding CharacterSet in Swift": [Link](https://flaviocopes.com/swift-character-set/)