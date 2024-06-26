---
date: 2024-01-26 01:17:13.342775-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: \u090F\u0915 \u0915\
  \u093E\u0930\u094D\u092F \u0915\u0940 \u0915\u0932\u094D\u092A\u0928\u093E \u0915\
  \u0930\u0947\u0902: \u090F\u0915 \u0910\u0930\u0947 \u0915\u093E \u0914\u0938\u0924\
  \ \u0928\u093F\u0915\u093E\u0932\u0928\u093E\u0964 \u092C\u093F\u0928\u093E \u092B\
  \u093C\u0902\u0915\u094D\u0936\u0928 \u0915\u0947, \u0906\u092A \u0907\u0938\u0947\
  \ \u0938\u092D\u0940 \u092E\u0947\u0928 \u092E\u0947\u0902 \u0930\u0916\u0947\u0902\
  \u0917\u0947\u0964 \u092B\u093C\u0902\u0915\u094D\u0936\u0928 \u0915\u0947 \u0938\
  \u093E\u0925, \u0906\u092A \u0907\u0938\u0947 \u0907\u0938 \u0924\u0930\u0939 \u0915\
  \u0930\u0947\u0902\u0917\u0947."
lastmod: '2024-03-13T22:44:52.927826-06:00'
model: gpt-4-0125-preview
summary: "\u090F\u0915 \u0915\u093E\u0930\u094D\u092F \u0915\u0940 \u0915\u0932\u094D\
  \u092A\u0928\u093E \u0915\u0930\u0947\u0902."
title: "\u0915\u094B\u0921 \u0915\u094B \u092B\u093C\u0902\u0915\u094D\u0936\u0928\
  \u094D\u0938 \u092E\u0947\u0902 \u0935\u094D\u092F\u0935\u0938\u094D\u0925\u093F\
  \u0924 \u0915\u0930\u0928\u093E"
weight: 18
---

## कैसे करें:
एक कार्य की कल्पना करें: एक ऐरे का औसत निकालना। बिना फ़ंक्शन के, आप इसे सभी मेन में रखेंगे। फ़ंक्शन के साथ, आप इसे इस तरह करेंगे:

```swift
func calculateAverage(of numbers: [Double]) -> Double {
    let sum = numbers.reduce(0, +)
    return numbers.isEmpty ? 0 : sum / Double(numbers.count)
}

// प्रयोग
let scores = [92.5, 88.75, 99.0, 70.5]
let averageScore = calculateAverage(of: scores)
print("औसत स्कोर है \(averageScore)")
```

नमूना आउटपुट होगा:
```
औसत स्कोर है 87.6875
```

## गहन विवेचन
इतिहासिक रूप से, जैसे जैसे प्रोग्रामिंग जटिल हुई, फ़ंक्शन जटिलता को प्रबंधित करने के लिए एक मुख्य आधार बन गए। विकल्पों में इनलाइन कोडिंग और कोड की कॉपी-पेस्टिंग (स्पेघट्टी कोड) शामिल हैं - अब बड़े पैमाने पर खराब प्रैक्टिस मानी जाती है। Swift में, फ़ंक्शन प्रथम-श्रेणी के नागरिक होते हैं; उन्हें वेरिएबल को सौंपा जा सकता है, तर्क के रूप में दिया जा सकता है, और अन्य फ़ंक्शन से रिटर्न किया जा सकता है, जिससे कोड अधिक मॉड्यूलर और लचीला बनता है।

कार्यान्वयन के लिहाज से, अपने फ़ंक्शन को एक चीज़ को अच्छी तरह से करने के लिए डिज़ाइन करें। एक स्पष्ट उद्देश्य और उसके नाम को दर्शाने वाले फ़ंक्शन का लक्ष्य रखें। पैरामीटर की संख्या पर ध्यान दें-बहुत सारे होने पर आप संभवतः बहुत अधिक काम कर रहे हैं। त्रुटि हैंडलिंग? फेंकने वाले फ़ंक्शन पर विचार करें और समस्याओं को अनुग्रहपूर्वक संभालें। याद रखें: Swift सभी पठनीयता और रख-रखाव की सुविधा के बारे में है।

## यह भी देखें
- [स्विफ्ट प्रोग्रामिंग भाषा गाइड - फ़ंक्शन](https://docs.swift.org/swift-book/LanguageGuide/Functions.html)
- [रे वेंडरलिच की स्विफ्ट स्टाइल गाइड](https://github.com/raywenderlich/swift-style-guide)
- [मार्टिन फॉलर का रिफैक्टरिंग: विद्यमान कोड के डिज़ाइन में सुधार](https://martinfowler.com/books/refactoring.html)
