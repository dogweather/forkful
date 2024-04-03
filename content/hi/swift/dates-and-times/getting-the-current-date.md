---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:12:05.465586-07:00
description: "\u0915\u0948\u0938\u0947: Swift \u0915\u093E `Foundation` \u0922\u093E\
  \u0902\u091A\u093E `Date` \u0935\u0930\u094D\u0917 \u092A\u094D\u0930\u0926\u093E\
  \u0928 \u0915\u0930\u0924\u093E \u0939\u0948, \u091C\u093F\u0938\u0938\u0947 \u0935\
  \u0930\u094D\u0924\u092E\u093E\u0928 \u0924\u093E\u0930\u0940\u0916 \u0914\u0930\
  \ \u0938\u092E\u092F \u092A\u094D\u0930\u093E\u092A\u094D\u0924 \u0915\u0930\u0928\
  \u093E \u0938\u0930\u0932 \u0939\u094B \u091C\u093E\u0924\u093E \u0939\u0948\u0964\
  \ \u092F\u0939\u093E\u0901 \u0935\u0930\u094D\u0924\u092E\u093E\u0928 \u0924\u093E\
  \u0930\u0940\u0916 \u092A\u094D\u0930\u093E\u092A\u094D\u0924 \u0915\u0930\u0928\
  \u0947 \u0915\u093E\u2026"
lastmod: '2024-03-13T22:44:52.936295-06:00'
model: gpt-4-0125-preview
summary: "Swift \u0915\u093E `Foundation` \u0922\u093E\u0902\u091A\u093E `Date` \u0935\
  \u0930\u094D\u0917 \u092A\u094D\u0930\u0926\u093E\u0928 \u0915\u0930\u0924\u093E\
  \ \u0939\u0948, \u091C\u093F\u0938\u0938\u0947 \u0935\u0930\u094D\u0924\u092E\u093E\
  \u0928 \u0924\u093E\u0930\u0940\u0916 \u0914\u0930 \u0938\u092E\u092F \u092A\u094D\
  \u0930\u093E\u092A\u094D\u0924 \u0915\u0930\u0928\u093E \u0938\u0930\u0932 \u0939\
  \u094B \u091C\u093E\u0924\u093E \u0939\u0948\u0964 \u092F\u0939\u093E\u0901 \u0935\
  \u0930\u094D\u0924\u092E\u093E\u0928 \u0924\u093E\u0930\u0940\u0916 \u092A\u094D\
  \u0930\u093E\u092A\u094D\u0924 \u0915\u0930\u0928\u0947 \u0915\u093E \u090F\u0915\
  \ \u092E\u0942\u0932 \u0909\u0926\u093E\u0939\u0930\u0923 \u0939\u0948."
title: "\u0935\u0930\u094D\u0924\u092E\u093E\u0928 \u0924\u093E\u0930\u0940\u0916\
  \ \u092A\u094D\u0930\u093E\u092A\u094D\u0924 \u0915\u0930\u0928\u093E"
weight: 29
---

## कैसे:
Swift का `Foundation` ढांचा `Date` वर्ग प्रदान करता है, जिससे वर्तमान तारीख और समय प्राप्त करना सरल हो जाता है। यहाँ वर्तमान तारीख प्राप्त करने का एक मूल उदाहरण है:

```swift
import Foundation

let currentDate = Date()
print(currentDate)
```

इसका उत्पादन कुछ इस तरह होगा:

```
2023-04-12 07:46:23 +0000
```

उत्पादन प्रारूप ISO 8601 मानक का अनुसरण करता है, UTC समय क्षेत्र का उपयोग करते हुए। हालाँकि, आप इस तारीख को प्रदर्शन उद्देश्यों के लिए स्वरूपित करना चाह सकते हैं। Swift का `DateFormatter` वर्ग इसमें सहायता प्रदान करता है:

```swift
let formatter = DateFormatter()
formatter.dateStyle = .long
formatter.timeStyle = .medium
let formattedDate = formatter.string(from: currentDate)
print(formattedDate)
```

नमूना उत्पादन हो सकता है:

```
April 12, 2023 को 10:46:23 AM
```

नोट करें कि उत्पादन प्रारूप कोड चल रहे डिवाइस के लोकेल पर निर्भर करेगा।

अधिक जटिल तारीख संचालन की आवश्यकता वाले प्रोजेक्ट्स के लिए, कई Swift डेवलपर्स जैसे कि `SwiftDate` पर तीसरे पक्ष की लाइब्रेरी की ओर रुख करते हैं। `SwiftDate` का उपयोग करके आप विशेष टाइम ज़ोन और स्वरूप में वर्तमान तारीख कैसे प्राप्त कर सकते हैं, इसका यहाँ एक उदाहरण है:

पहले, SPM, CocoaPods, या Carthage का उपयोग करके अपनी परियोजना में `SwiftDate` को जोड़ें। फिर:

```swift
import SwiftDate

let rome = Region(calendar: .gregorian, zone: .europeRome, locale: .current)
let currentDateInRome = DateInRegion(Date(), region: rome)
print(currentDateInRome.toFormat("yyyy-MM-dd HH:mm:ss"))
```

इससे उत्पादन हो सकता है:

```
2023-04-12 09:46:23
```

`SwiftDate` का उपयोग करके, आप विभिन्न समय क्षेत्रों और स्थानों के लिए तारीखों और समयों को आसानी से मैनिप्युलेट कर सकते हैं, जिससे आपके Swift अनुप्रयोगों में जटिल तारीख संचालन कार्यों को सरल बनाया जा सकता है।
