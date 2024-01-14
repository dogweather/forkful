---
title:                "Swift: स्ट्रिंग को छोटे अक्षर में रूपांतरित करना"
programming_language: "Swift"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## क्यों

कोडिंग में स्ट्रिंग को लोअर केस में बदलने का उद्देश्य हिंदी पाठकों को साथ देगा।

## कैसे करें

```Swift
let str = "HELLO"
let lowerCaseStr = str.lowercased()
print(lowerCaseStr)

// Output: hello
```

यहां हमने `lowercased()` फ़ंक्शन का इस्तेमाल किया है जो हमारी स्ट्रिंग को लोअर केस में बदल देता है। यह बहुत ही आसान और स्पष्ट है, जो हमारे कोड को और भी सरल बनाता है।

## गहराई में जाएं

स्ट्रिंग को लोअर केस में बदलने का एक अन्य तरीका `lowercased()` के साथ स्ट्रिंग को स्प्लिट करके कैसे किया जा सकता है। इसमें हम `components(separatedBy:)` फ़ंक्शन का इस्तेमाल करेंगे जो स्पेस द्वारा स्प्लिट करता है और लोअर केस में बदल देता है।

```Swift
let str = "HELLO WORLD"
let lowerCaseStr = str.components(separatedBy: " ").map{$0.lowercased()}.joined(separator: " ")
print(lowerCaseStr)

// Output: hello world
```

यह एक थोड़ी लंबी प्रक्रिया है, लेकिन इससे हमारी स्ट्रिंग में दिए गए स्पेस को क्रॉस चेक करते हुए स्पष्ट लोअर केस में बदल सकते हैं।

## देखें भी

- [Apple Documentation on `lowercased()`](https://developer.apple.com/documentation/swift/string#//apple_ref/swift/string/methods/lowercased)

- [Apple Documentation on `components(separatedBy:)`](https://developer.apple.com/documentation/swift/string/3126538-components)

- [Hacking with Swift - How to lowercase a string](https://www.hackingwithswift.com/example-code/strings/how-to-lowercase-a-string-using-localizedcapitalized-and-lowercased)

यहां आप अधिक सीख सकते हैं कि स्ट्रिंग को लोअर केस में बदलने का और भी तरीके क्या हो सकते हैं और दोनों फ़ंक्शन को समझने के लिए और सुविधाएँ कैसे उनको उपयोग में ले सकते हैं।