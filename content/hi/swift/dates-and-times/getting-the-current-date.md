---
title:                "वर्तमान तारीख प्राप्त करना"
date:                  2024-02-03T19:12:05.465586-07:00
model:                 gpt-4-0125-preview
simple_title:         "वर्तमान तारीख प्राप्त करना"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?
Swift में वर्तमान तारीख प्राप्त करने के लिए `Date` वर्ग का उपयोग उस समय और तारीख तक पहुँचने के लिए किया जाता है जब ऐप चलाई जा रही है। प्रोग्रामरों को विभिन्न कारणों से वर्तमान तारीख प्राप्त करने की आवश्यकता होती है जो समयसीमा की घटनाओं को मुद्रांकन करने, तारीख की गणना करने, तारीखों और समय को एक उपयोगकर्ता इंटरफेस में प्रदर्शित करने से लेकर होती है।

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
