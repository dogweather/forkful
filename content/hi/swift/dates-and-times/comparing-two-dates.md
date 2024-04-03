---
date: 2024-01-20 17:34:21.013096-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) ."
lastmod: '2024-03-13T22:44:52.939716-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u0926\u094B \u0924\u093E\u0930\u0940\u0916\u094B\u0902 \u0915\u0940 \u0924\
  \u0941\u0932\u0928\u093E"
weight: 27
---

## How to: (कैसे करें:)
```Swift
import Foundation

// दो Dates बनाएं
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "yyyy-MM-dd"
let firstDate = dateFormatter.date(from: "2023-01-01")!
let secondDate = dateFormatter.date(from: "2023-01-05")!

// तारीखों की तुलना करें
if firstDate == secondDate {
    print("तारीखें समान हैं")
} else if firstDate < secondDate {
    print("पहली तारीख दूसरी तारीख से पहले है")
} else {
    print("पहली तारीख दूसरी तारीख के बाद है")
}

// सैंपल आउटपुट: पहली तारीख दूसरी तारीख से पहले है
```

## Deep Dive (गहराई में जानकारी)
स्विफ्ट में, `Date` ऑब्जेक्ट्स को तुलना करना `Equatable` और `Comparable` प्रोटोकॉल्स की मदद से होता है। ये प्रोटोकॉल्स आपको `==`, `<`, `>`, `<=`, और `>=` ऑपरेटर्स से तारीखों की तुलना करने को देते हैं। पीछे स्विफ्ट की TimeInterval का उपयोग करके दो डेट्स के बीच के सेकंड्स की तुलना की जाती है। इस कार्य के विकल्प में `Calendar` API भी है, जो अधिक जटिल तारीख संबंधित ऑपरेशन्स को हैंडल कर सकता है।

## See Also (और भी देखें)
- [Apple: Working with Dates and Times](https://developer.apple.com/documentation/foundation/date)
