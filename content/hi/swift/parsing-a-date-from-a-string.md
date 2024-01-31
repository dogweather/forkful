---
title:                "स्ट्रिंग से दिनांक पार्स करना"
date:                  2024-01-20T15:39:26.212596-07:00
html_title:           "Arduino: स्ट्रिंग से दिनांक पार्स करना"
simple_title:         "स्ट्रिंग से दिनांक पार्स करना"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
डेट को स्ट्रिंग से पार्स करना मतलब टेक्स्ट फॉर्म में दी गई तारीख को प्रोग्राम के समझने योग्य फॉर्मेट में बदलना है। प्रोग्रामर्स इसे इसलिए करते हैं ताकि तारीख से संबंधित क्रियावलियां (जैसे कि तुलना करना, समय क्षेत्र में बदलाव करना) को आसान बनाया जा सके।

## How to (कैसे करें):
Swift में `DateFormatter` क्लास का उपयोग करके हम आसानी से एक स्ट्रिंग को डेट में पार्स कर सकते हैं।

```swift
import Foundation

let dateString = "2023-04-02"
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "yyyy-MM-dd"

if let date = dateFormatter.date(from: dateString) {
    print("डेट पार्स हो गई: \(date)")
} else {
    print("गलत फॉर्मेट है।")
}
```

इस कोड से आपको आउटपुट मिलेगा जो `Date` ऑब्जेक्ट को प्रिंट करेगा अगर पार्सिंग सही से हुई हो। 

## Deep Dive (गहराई से जानकारी):
डेट पार्सिंग की महत्ता तब से है जब से प्रोग्रामिंग शुरू हुई, क्योंकि तारीख की मानकीकरण और विभिन्न फॉर्मेटों की अदला-बदली जरूरत बहुत आम है। `DateFormatter` के अलावा, Swift में `ISO8601DateFormatter` जैसे स्पेशलाइज्ड फॉर्मेटर्स भी हैं जो ISO 8601 स्टैंडर्ड फॉर्मेट में डेट को पार्स करने के लिए उपयोग होते हैं।

Implementation details में `dateFromString` फंक्शन लोकल टाइमज़ोन में डेट ऑब्जेक्ट को इंटरप्रेट करता है, जब तक कि एक विशेष `timeZone` सेट न किया गया हो। दुनियाभर में समय क्षेत्रों की विविधता के कारण, यह सुनिश्चित करना जरूरी है कि स्ट्रिंग से पार्स की गई डेट सही टाइमज़ोन में हो।

## See Also (इसे भी देखें):
- Swift के `Date` और `DateFormatter` के लिए अधिकारी दस्तावेज़ (Documentation): [Date](https://developer.apple.com/documentation/foundation/date) और [DateFormatter](https://developer.apple.com/documentation/foundation/dateformatter)
- Apple का Date Formatting Guide: [Date Formatters](https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/DataFormatting/Articles/dfDateFormatting10_4.html)
- `ISO8601DateFormatter` के बारे में Apple का दस्तावेज़: [ISO8601DateFormatter](https://developer.apple.com/documentation/foundation/iso8601dateformatter)
