---
title:                "स्ट्रिंग की लंबाई का पता लगाना"
html_title:           "Swift: स्ट्रिंग की लंबाई का पता लगाना"
simple_title:         "स्ट्रिंग की लंबाई का पता लगाना"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## क्यों 
कोई भी प्रोग्रामिंग भाषा में प्रवेश करने के लिए, दैसम आदतें बनाना बहुत महत्वपूर्ण है। शुरुआती स्तर पर स्ट्रिंग की लंबाई को पता करना इसी दैसम में समर्थ है और यह स्विफ्ट को सीखने का एक शानदार तरीका है।

## कैसे करें 
कोई भी स्ट्रिंग की लंबाई को पता करने के लिए, आपके पास दो विकल्प हैं। पहला तरीका है आपके पास "count" विधि है जो स्ट्रिंग के अंत में उसकी चर संख्या वापस देती है। दूसरा तरीका है "length" विधि जो स्ट्रिंग के अंत में लगे "characters" की संख्या वापस देती है। नीचे दी गई उदाहरण में हमने दोनों विकल्प का उपयोग करके स्ट्रिंग की लंबाई को प्रिंट किया है। 

```Swift
let str = "हिन्दी"
print(str.count) // 5
print(str.characters.length) // 5
```

## गहराई में जाएं 
स्ट्रिंग की लंबाई को पता करना स्विफ्ट में बहुत आसान है, लेकिन इसमें कुछ गहराई भी है। जब हम "count" विधि का उपयोग करते हैं तो उससे वापस देशित भाषा में लंबाई नहीं मिलती है, बल्कि चरों की संख्या मिलती है। इसका मतलब है कि अगर हमारे स्ट्रिंग में कोई शून्य भी हो, तो भी यह "count" में शामिल होगा। जबकि "characters.length" विधि स्ट्रिंग के अंत में लगे अक्षरों की संख्या ही देती है। 

## देखें भी 
- [Swift Strings Tutorial](https://www.raywenderlich.com/51409/working-with-strings-in-swift)
- [String Methods](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID272)