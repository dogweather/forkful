---
title:                "स्ट्रिंग की लंबाई पता करना"
html_title:           "C++: स्ट्रिंग की लंबाई पता करना"
simple_title:         "स्ट्रिंग की लंबाई पता करना"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों? (What & Why?)

स्ट्रिंग की लंबाई ढूंढना उसके वर्णों की संख्या जानना होता हैं। कार्यक्रमकर्ता इसे तभी करते हैं जब उन्हें जानकारी का माप लेना होता है या उन्हें किसी विशेष मापदंड पर आधारित कार्य होता है।

## कैसे करें: (How to:)

```swift
let myString = "नमस्ते, स्विफ्ट!"
let length = myString.count
print("स्ट्रिंग की लंबाई: \(length)")
```
__आउटपुट:__
```
स्ट्रिंग की लंबाई: 13
```

## गहन अध्ययन: (Deep Dive):

(1) हिस्टोरिकल कन्टेक्स्ट: 'स्ट्रिंग लेंग्थ' का उपयोग केवल स्विफ्ट में ही नहीं होता। यह कंसेप्ट कंप्यूटर साइंस के पहले दिनों से मौजूद था। 

(2) वैकल्पिक: आप भी 'for-in' loop का उपयोग करके स्ट्रिंग की लंबाई जांच सकते हैं, लेकिन स्ट्रिंग का `.count` फ़ंक्शन इसे आसान और त्वरित बनाता है. 

(3) आवेदन विवरण: स्विफ्ट का `.count` प्रॉपर्टी unicode स्केलर वेल्यूज़ की गिनती करता हैं, जो कि उपयोगी हो सकता हैं जब आपको संकेत के बजाय यूनिकोड वर्णों की गिनती करनी हो।

## यह भी देखें: (See Also):

2. यूनिकोड स्केलर वेल्यूज़ के बारे में अधिक जानकारी: [https://www.unicode.org/glossary/#unicode_scalar_value](https://www.unicode.org/glossary/#unicode_scalar_value)