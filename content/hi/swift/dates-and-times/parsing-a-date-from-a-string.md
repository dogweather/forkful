---
title:                "स्ट्रिंग से तारीख पार्स करना"
aliases:
- /hi/swift/parsing-a-date-from-a-string.md
date:                  2024-02-03T19:16:43.436276-07:00
model:                 gpt-4-0125-preview
simple_title:         "स्ट्रिंग से तारीख पार्स करना"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?
एक स्ट्रिंग से दिनांक पार्स करना टेक्स्टुअल दिनांक और समय प्रतिनिधित्वों को `Date` ऑब्जेक्ट में बदलने की प्रक्रिया है। यह प्रक्रिया उन अनुप्रयोगों में आवश्यक है जहां दिनांकों को स्ट्रिंग्स के रूप में संवादित किया जाता है, जैसे कि API प्रतिक्रियाओं या उपयोगकर्ता इनपुट्स में, जिससे दिनांक मैनिपुलेशन और फॉर्मेटिंग आसान हो जाती है।

## कैसे:

### फाउंडेशन का `DateFormatter` का उपयोग करना
स्विफ्ट की स्टैंडर्ड लाइब्रेरी, फाउंडेशन, `DateFormatter` प्रदान करती है जो स्ट्रिंग्स को `Date` ऑब्जेक्ट में और विपरीत बदलने के लिए है। एक स्ट्रिंग से दिनांक पार्स करने के लिए, आप स्ट्रिंग से मेल खाने वाले दिनांक फॉर्मेट को निर्दिष्ट करते हैं, फिर इसे पार्स करने के लिए फॉर्मेटर का उपयोग करते हैं।

```swift
import Foundation

let dateString = "2023-04-30"
let formatter = DateFormatter()
formatter.dateFormat = "yyyy-MM-dd"
if let date = formatter.date(from: dateString) {
    print("पार्स की गई दिनांक: \(date)")
} else {
    print("दिनांक पार्स करने में विफल")
}
// नमूना आउटपुट: पार्स की गई दिनांक: 2023-04-29 22:00:00 +0000
```

नोट करें कि आउटपुट आपके टाईमज़ोन के आधार पर भिन्न हो सकता है।

### ISO8601DateFormatter का उपयोग करना
ISO 8601 दिनांक प्रारूपों के लिए, स्विफ्ट एक विशेषज्ञ फॉर्मेटर, `ISO8601DateFormatter` प्रदान करता है, जो पार्सिंग प

्रक्रिया को सरल बनाता है।

```swift
import Foundation

let dateString = "2023-04-30T15:00:00+00:00"
let isoFormatter = ISO8601DateFormatter()
if let date = isoFormatter.date(from: dateString) {
    print("पार्स की गई ISO8601 दिनांक: \(date)")
} else {
    print("ISO8601 दिनांक पार्स करने में विफल")
}
// नमूना आउटपुट: पार्स की गई ISO8601 दिनांक: 2023-04-30 15:00:00 +0000
```

### थर्ड-पार्टी लाइब्रेरी का उपयोग: SwiftDate
जबकि स्विफ्ट दिनांक पार्स करने के लिए शक्तिशाली उपकरण प्रदान करता है, थर्ड-पार्टी लाइब्रेरीज़ जैसे कि SwiftDate और भी अधिक लचीलापन और सुविधा प्रदान करती हैं। अपने प्रोजेक्ट में SwiftDate जोड़ने के बाद, पार्सिंग उतनी ही साधारण हो जाती है:

```swift
import SwiftDate

let dateString = "April 30, 2023"
if let date = dateString.toDate("MMMM dd, yyyy") {
    print("SwiftDate के साथ पार्स की गई दिनांक: \(date)")
} else {
    print("SwiftDate के साथ दिनांक पार्स करने में विफल")
}
// नमूना आउटपुट: SwiftDate के साथ पार्स की गई दिनांक: 2023-04-30 00:00:00 +0000
```

SwiftDate प्राकृतिक भाषा और व्यापक रेंज के दिनांक प्रारूपों के साथ पार्सिंग को सरल बनाता है, जिससे यह आपके स्विफ्ट प्रोग्रामिंग टूलकिट को शक्तिशाली अतिरिक्त बनाता है।
