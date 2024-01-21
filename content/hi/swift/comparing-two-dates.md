---
title:                "दो तारीखों की तुलना"
date:                  2024-01-20T17:34:21.013096-07:00
model:                 gpt-4-1106-preview
simple_title:         "दो तारीखों की तुलना"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/comparing-two-dates.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
तारीखों की तुलना करने का मतलब है पता करना कि एक तारीख दूसरी से पहले है, बाद में है या दोनों समान हैं। प्रोग्रामर्स यह तय करने के लिए तारीखों की तुलना करते हैं कि कोई इवेंट हो चुकी है या आने वाली है, और समय से संबंधित फीचर्स को हैंडल करने के लिए।

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