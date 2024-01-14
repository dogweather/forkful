---
title:                "Swift: उप-स्ट्रिंग निकालना"
simple_title:         "उप-स्ट्रिंग निकालना"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/extracting-substrings.md"
---

{{< edit_this_page >}}

## क्यों

स्विफ्ट प्रोग्रामिंग बहुत ही व्यापक है और इसमें कई काम किए जा सकते हैं, जैसे स्ट्रिंग्स को अलग से निकालना। यह अपने दस्तावेजों और डेटा को अधिक सुविधाजनक तरीके से संगठित करने में मदद कर सकता है।

## कैसे करें

स्ट्रिंग्स से सबस्ट्रिंग्स निकालने का सबसे सरल तरीका `suffix()` और `prefix()` फ़ंक्शन्स का उपयोग करना है। यहाँ एक उदाहरण है:

```Swift 
let name = "John Doe"
let lastName = name.suffix(3)
print(lastName)
// Output: "Doe"
```

आप `substring()` फ़ंक्शन का उपयोग करके भी सबस्ट्रिंग्स निकाल सकते हैं। इसमें आप को सबस्ट्रिंग की शुरुआत की और अंत की इंडेक्स भी देनी होगी। यहाँ एक उदाहरण है:

```Swift 
let sentence = "Hello, how are you?"
let greeting = sentence.substring(with: 0..<5)
print(greeting)
// Output: "Hello"
```

## गहराई में जाएं

सबस्ट्रिंग्स को अलग करने के लिए इस्तेमाल किए जाने वाले फ़ंक्शन्स का अधिक गहराई से समझना बेहद महत्वपूर्ण है। इसके अलावा, यह प्रोग्रामिंग में प्रयोग करने में और भी कुछ तरीके हैं, जो आपके कोड को और भी बेहतर बना सकते हैं। स्ट्रिंग्स के साथ काम करते समय सावधान रहें और समस्याओं को सुलझाने के लिए इस्तेमाल करीबी रखें।

## देखें भी

- [Swift Strings Tutorial](https://www.appcoda.com/swift-string/)
- [String Manipulation in Swift](https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/Strings/Articles/using.html#//apple_ref/doc/uid/TP40004265-SW1)
- [Swift String Cheat Sheet](https://learnappmaking.com/swift-string-cheat-sheet/)
- [Advanced String Manipulation in Swift](https://www.ralfebert.de/ios/extras/string-processing/)