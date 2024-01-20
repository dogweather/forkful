---
title:                "स्ट्रिंग को कैपिटलाइज़ करना"
html_title:           "C: स्ट्रिंग को कैपिटलाइज़ करना"
simple_title:         "स्ट्रिंग को कैपिटलाइज़ करना"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)

स्ट्रिंग को कैपिटलाइज़ करना मतलब है हर शब्ड के पहले अक्षर को बड़ा (अपरकेस) करना. प्रोग्रामर्स यह तब करते हैं जब उन्हें टेक्स्ट को औपचारिक या आकर्षक दिखाना होता है, जैसे कि शीर्षकों या नामों में.

## How to: (कैसे करें:)

Swift में, आप `capitalized` प्रॉपर्टी का इस्तेमाल करके स्ट्रिंग को कैपिटलाइज़ कर सकते हैं:

```Swift
let sentence = "यह एक उदाहरण वाक्य है."
let capitalizedSentence = sentence.capitalized
print(capitalizedSentence)
```

सैंपल आउटपुट:

```
यह एक उदाहरण वाक्य है.
```

सिर्फ पहला अक्षर कैपिटलाइज़ करने के लिए:

```Swift
let word = "स्विफ्ट"
let capitalizedWord = word.prefix(1).uppercased() + word.dropFirst()
print(capitalizedWord)
```

सैंपल आउटपुट:

```
स्विफ्ट
```

## Deep Dive (गहराई में जानकारी)

पहले, Swift में `capitalized` नहीं था, केवल `uppercaseString` और `lowercaseString` थे. समय के साथ, `capitalized` जोड़ा गया ताकि विशेष रूप से प्रत्येक शब्द को कैपिटलाइज़ किया जा सके. ऑल्टरनेटिव में, आप एक फंक्शन भी बना सकते हैं जो `string` को लूप करके हर शब्द के पहले अक्षर को बड़ा कर दे.

कैपिटलाइज़ करना आमतौर पर यूनिकोड स्कैलर्स का प्रयोग करते हुए होता है. यूनिकोड स्टैंडर्ड में प्रत्येक अक्षर या ग्लिफ के लिए एक अद्वितीय नंबर होता है. जब आप कोई स्ट्रिंग कैपिटलाइज़ करते हैं, स्विफ्ट इंटरनली उन्हें उनके अपरकेस वर्शन में तब्दील कर देता है.

## See Also (और देखें)

- [Swift Documentation on Strings](https://developer.apple.com/documentation/swift/string)
- [Unicode.org](http://www.unicode.org/)
- [Swift String and Characters](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)