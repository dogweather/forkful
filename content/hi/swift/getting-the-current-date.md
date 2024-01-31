---
title:                "वर्तमान तारीख प्राप्त करना"
date:                  2024-01-20T15:17:08.296918-07:00
simple_title:         "वर्तमान तारीख प्राप्त करना"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)

स्विफ्ट में वर्तमान तारीख प्राप्त करना यह दर्शाता है कि आप किसी भी समय कोड को चलाने पर उस समय की तारीख कैसे प्राप्त कर सकते हैं। प्रोग्रामर लॉगिंग, टाइमस्टैम्प्स और डेटा प्रोसेसिंग के लिए अक्सर यह काम करते हैं।

## How to: (कैसे करें:)

स्विफ्ट कोड में वर्तमान तारीख प्राप्त करने का उदाहरण इस प्रकार है:

```Swift
import Foundation

let currentDate = Date()
print(currentDate)
```

उदाहरण का आउटपुट कुछ इस प्रकार होगा (वास्तविक आउटपुट वर्तमान तारीख और समय पर निर्भर करेगा):

```
2023-04-05 12:00:00 +0000
```

## Deep Dive (गहराई से जानकारी):

Date ऑब्जेक्ट स्विफ्ट में NSDate का नाम बदलने के साथ आया है, जो ओरिजिनली Objective-C से लिया गया है। Foundation फ्रेमवर्क में इस्तेमाल किया जाने वाला Date वास्तव में एक बिंदु होता है जो 1 जनवरी 2001 को ग्रीनविच मीन टाइम (GMT) पर आधारित होता है। विकल्प में DateFormatter का इस्तेमाल भी हो सकता है जो तारीख और समय को मनचाहे फॉर्मैट में बदलने की सुविधा देता है।

## See Also (और देखें):

- Apple का ऑफिशल डॉक्यूमेंटेशन फॉर Date: [Apple Documentation - Date](https://developer.apple.com/documentation/foundation/date)
- DateFormatter के उपयोग पर गाइड: [Date Formatting in Swift](https://nsdateformatter.com/)
