---
title:                "तारीख को स्ट्रिंग में बदलना"
date:                  2024-01-20T17:37:58.061310-07:00
model:                 gpt-4-1106-preview
simple_title:         "तारीख को स्ट्रिंग में बदलना"

category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

डेट को स्ट्रिंग में बदलना मतलब है तारीख और समय को पठनीय फॉर्मेट में परिवर्तित करना। प्रोग्रामर्स इसे इसलिए करते हैं ताकि एप्लिकेशन यूजर्स आसानी से तारीख और समय को समझ सकें और स्क्रीन पर सही तरीके से प्रदर्शित हो।

## कैसे करें:

Swift में `DateFormatter` का इस्तेमाल करते हुए डेट को स्ट्रिंग में बदल सकते हैं। नीचे कोड दिया गया है:

```Swift
import Foundation

let currentDate = Date()

let formatter = DateFormatter()
formatter.dateFormat = "dd/MM/yyyy"

let dateString = formatter.string(from: currentDate)
print(dateString)
```

अगर आज 12 अप्रैल 2023 है, आउटपुट होगा: 
```
12/04/2023
```

## गहराई में जानकारी:

`DateFormatter` में `dateFormat` का इस्तेमाल करके डेट को विभिन्न प्रकार के स्ट्रिंग फॉर्मेट्स में प्रदर्शित किया जा सकता है। `DateFormatter` कौन्सेप्ट 1980s से है, जब आईबीएम के सिस्टम/३८ में समान उपकरण थे। वैकल्पिक रूप से, `ISO8601DateFormatter` का उपयोग करके ISO-8601 मानक तारीखें बनाई जा सकती हैं। Swift में `Date` और `String` के बीच परिवर्तन कई तरीकों से किया जा सकता है, जिसमें `.timeIntervalSince1970` या कस्टम पैर्टन्स जैसे `"EEEE, MMM d, yyyy"` भी शामिल हैं।

## संबंधित सूत्रों की जानकारी:

- [Apple's Date Formatting Guide](https://developer.apple.com/documentation/foundation/dateformatter)
- [Swift Documentation: DateFormatter](https://developer.apple.com/documentation/foundation/dateformatter)
