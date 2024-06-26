---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:22:51.028250-07:00
description: "\u0915\u0948\u0938\u0947: Swift \u092E\u0947\u0902, CSV \u092B\u093C\
  \u093E\u0907\u0932\u094B\u0902 \u0915\u094B \u0938\u0940\u0927\u0947 \u092A\u093E\
  \u0930\u094D\u0938 \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F \u092E\
  \u0942\u0932 \u0938\u092E\u0930\u094D\u0925\u0928 \u0928\u0939\u0940\u0902 \u0939\
  \u0948, \u0932\u0947\u0915\u093F\u0928 \u0906\u092A \u0938\u093E\u092E\u0917\u094D\
  \u0930\u0940 \u0915\u094B \u0935\u093F\u092D\u093E\u091C\u093F\u0924 \u0915\u0930\
  \u0928\u0947 \u0915\u0947 \u0932\u093F\u090F `String` \u0935\u093F\u0927\u093F\u092F\
  \u094B\u0902 \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\u0915\u0947\
  \ \u092F\u093E\u2026"
lastmod: '2024-03-13T22:44:52.956734-06:00'
model: gpt-4-0125-preview
summary: "Swift \u092E\u0947\u0902, CSV \u092B\u093C\u093E\u0907\u0932\u094B\u0902\
  \ \u0915\u094B \u0938\u0940\u0927\u0947 \u092A\u093E\u0930\u094D\u0938 \u0915\u0930\
  \u0928\u0947 \u0915\u0947 \u0932\u093F\u090F \u092E\u0942\u0932 \u0938\u092E\u0930\
  \u094D\u0925\u0928 \u0928\u0939\u0940\u0902 \u0939\u0948, \u0932\u0947\u0915\u093F\
  \u0928 \u0906\u092A \u0938\u093E\u092E\u0917\u094D\u0930\u0940 \u0915\u094B \u0935\
  \u093F\u092D\u093E\u091C\u093F\u0924 \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\
  \u093F\u090F `String` \u0935\u093F\u0927\u093F\u092F\u094B\u0902 \u0915\u093E \u0909\
  \u092A\u092F\u094B\u0917 \u0915\u0930\u0915\u0947 \u092F\u093E SwiftCSV \u091C\u0948\
  \u0938\u0940 \u0924\u0943\u0924\u0940\u092F-\u092A\u0915\u094D\u0937 \u092A\u0941\
  \u0938\u094D\u0924\u0915\u093E\u0932\u092F\u094B\u0902 \u0915\u093E \u0932\u093E\
  \u092D \u0909\u0920\u093E\u0915\u0930 CSV \u0921\u0947\u091F\u093E \u0915\u094B\
  \ \u0938\u0902\u092D\u093E\u0932 \u0938\u0915\u0924\u0947 \u0939\u0948\u0902 \u091C\
  \u094B \u090F\u0915 \u0905\u0927\u093F\u0915 \u0938\u0941\u0917\u092E \u0926\u0943\
  \u0937\u094D\u091F\u093F\u0915\u094B\u0923 \u092A\u094D\u0930\u0926\u093E\u0928\
  \ \u0915\u0930\u0924\u093E \u0939\u0948\u0964 \u092F\u0939\u093E\u0901 \u0926\u094B\
  \u0928\u094B\u0902 \u0935\u093F\u0927\u093F\u092F\u093E\u0901 \u0939\u0948\u0902\
  ."
title: "CSV \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\u092E \u0915\u0930\u0928\u093E"
weight: 37
---

## कैसे:
Swift में, CSV फ़ाइलों को सीधे पार्स करने के लिए मूल समर्थन नहीं है, लेकिन आप सामग्री को विभाजित करने के लिए `String` विधियों का उपयोग करके या SwiftCSV जैसी तृतीय-पक्ष पुस्तकालयों का लाभ उठाकर CSV डेटा को संभाल सकते हैं जो एक अधिक सुगम दृष्टिकोण प्रदान करता है। यहाँ दोनों विधियाँ हैं:

### बाहरी पुस्तकालयों के बिना मैन्युअल पार्सिंग
```swift
// एक सरल CSV स्ट्रिंग पर विचार करें
let csvString = """
name,age,city
John Doe,29,New York
Jane Smith,34,Los Angeles
"""

// CSV स्ट्रिंग को पंक्तियों में विभाजित करें
let rows = csvString.components(separatedBy: "\n")

// पहली पंक्ति से कीज़ निकालें
let keys = rows.first?.components(separatedBy: ",")

// दूसरी पंक्ति से शुरू होकर पंक्तियों पर इटरेट करें
var result: [[String: String]] = []
for row in rows.dropFirst() {
    let values = row.components(separatedBy: ",")
    let dict = Dictionary(uniqueKeysWithValues: zip(keys!, values))
    result.append(dict)
}

// नमूना आउटपुट
print(result)
// आउटपुट देता है: [{"city": "New York", "age": "29", "name": "John Doe"}, {"city": "Los Angeles", "age": "34", "name": "Jane Smith"}]
```
यह दृष्टिकोण सीधा है लेकिन विशेष मामलों जैसे कि मूल्यों में अल्पविराम, क्षेत्रों के भीतर लाइन ब्रेक्स आदि वाली CSV फ़ाइलों के साथ रोबस्टनेस की कमी होती है।

### SwiftCSV पुस्तकालय का उपयोग करना
पहले, अपनी परियोजना में SwiftCSV जोड़ें जिसे आपकी `Package.swift` निर्भरताओं में शामिल करके किया जा सकता है:
```swift
.package(url: "https://github.com/swiftcsv/SwiftCSV.git", from: "0.5.6")
```
फिर, इस प्रकार इसे आयात करें और उपयोग करें:
```swift
import SwiftCSV

// मान लें कि `csvString` उपरोक्त के रूप में परिभाषित है

// एक CSV ऑब्जेक्ट बनाएँ
if let csv = try? CSV(string: csvString) {
    // डिक्शनरीज के रूप में पंक्तियों तक पहुंचें
    let rows = csv.namedRows
    
    // नमूना आउटपुट
    print(rows)
    // आउटपुट देता है: [{"city": "New York", "age": "29", "name": "John Doe"}, {"city": "Los Angeles", "age": "34", "name": "Jane Smith"}]
}
```
SwiftCSV में समाहित अल्पविराम, क्षेत्रों में लाइन ब्रेक्स, और अक्षर एन्कोडिंग जैसे बारीकियों को स्वचालित रूप से सामना करके पार्सिंग को सरल बनाया जाता है। हालाँकि, वास्तविक दुनिया के अनुप्रयोगों में, विशेष रूप से बाहरी डेटा स्रोतों से निपटते समय संभावित त्रुटियों से निपटना याद रखें।
