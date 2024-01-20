---
title:                "एक स्ट्रिंग को लोअर केस में परिवर्तित करना"
html_title:           "Kotlin: एक स्ट्रिंग को लोअर केस में परिवर्तित करना"
simple_title:         "एक स्ट्रिंग को लोअर केस में परिवर्तित करना"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

स्पष्ट रूप से बताया जाए तो, स्ट्रिंग को लोअर केस में बदलना का अर्थ होता है स्ट्रिंग के सभी अक्षरों को छोटे (लोअर केस) अक्षरों में परिवर्तित करना। प्रोग्रामर्स इसे तब करते हैं जब उन्हें यह सुनिश्चित करना होता है कि स्ट्रिंग का संवेदनशीलता मामला में कोई भेदभाव नहीं हो, जैसे कि डेटा तुलना या खोज में।

## कैसे:

इसे Swift में करने के लिए, हमें `lowercased()` फ़ंक्शन का उपयोग करना होता है:

```Swift
let originalString = "Hello, Swift!"
let lowerCasedString = originalString.lowercased()
print(lowerCasedString)
```

उपरोक्त कोड का आउटपुट होगा:

```Swift
"hello, swift!"
```

## गहराई से जानिए:

1. **ऐतिहासिक प्रक्षेप**: Swift में `lowercased()` फ़ंक्शन को Apple ने विकसित किया है। इसका उपयोग स्ट्रिंग के सभी वर्णों को निचले मामले में परिवर्तित करने के लिए किया जाता है। 

2. **विकल्प**: अगर आपको एक खास वर्णमाला (जैसे कि टर्की) के लिए स्ट्रिंग को निचले मामले में बदलने की आवश्यकता है, तो आप `localizedLowercase` गुण का उपयोग कर सकते हैं। 

3. **कार्यान्वयन विवरण**: Swift में, `lowercased()` फ़ंक्शन Unicode संसाधनों का उपयोग करके स्ट्रिंग के हर केस का विमोचन करता है 

## और देखें:

2. Stack Overflow: [Converting String to Lower Case in Swift](https://stackoverflow.com/questions/26351055/swift-constant-conversion-to-lower-case)