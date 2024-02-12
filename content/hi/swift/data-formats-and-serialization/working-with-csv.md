---
title:                "CSV के साथ काम करना"
aliases:
- /hi/swift/working-with-csv.md
date:                  2024-02-03T19:22:51.028250-07:00
model:                 gpt-4-0125-preview
simple_title:         "CSV के साथ काम करना"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?

CSV (Comma-Separated Values) फ़ाइलों के साथ कार्य करना पाठ फ़ाइलों से संरचित डेटा को पार्स करने और उत्पन्न करने की प्रक्रिया होती है, जहाँ प्रत्येक पंक्ति एक रिकॉर्ड का प्रतिनिधित्व करती है और प्रत्येक रिकॉर्ड अल्पविरामों द्वारा विभाजित क्षेत्रों से मिलकर बनता है। प्रोग्रामर अक्सर इस गतिविधि में संलग्न होते हैं ताकि वे आसानी से तालिका डेटा को आयात कर सकें, निर्यात कर सकें और उसे विभिन्न मंचों और प्रोग्रामिंग भाषाओं में व्यापक रूप से समर्थित एक प्रारूप का उपयोग करके मैनिप्यूलेट कर सकें, इसकी सादगी और मानव-पठनीय प्रारूप के कारण।

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
