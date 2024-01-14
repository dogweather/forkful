---
title:    "Swift: दो तारीखों का तुलना करना"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/hi/swift/comparing-two-dates.md"
---

{{< edit_this_page >}}

## क्यों

दो तारीखों को तुलना करने में शामिल होने की *क्यों* करने के लिए कुछ महत्वपूर्ण कारण हैं। इसके अलावा, यह स्ट्रिंग, अखंड गणना, और डेटा संग्रहण में उपयोगी हो सकता है। हालांकि, इसके अतिरिक्त इसका उपयोग विविध संदर्भों में भी किया जा सकता है। 

## कैसे

इस स्विफ्ट प्रोग्राम में हम दो तारीखों को तुलना करने के लिए तीन अलग-अलग तरीकों को देखेंगे। पहले, हम ```compare (_: डेट, to: डेट, toGranularity: Calendar.Unit) फंक्शन``` का उपयोग करके तारीखों को तुलना करेंगे। दूसरे, हम ``` <``` और ```>``` ऑपरेटर का उपयोग करके तारीखों को तुलना करेंगे। तीसरा तरीका है ```isDate(_: equalTo: to Granularity:)``` फंक्शन का उपयोग करना जो तारीखों को तुलना करता है और विशिष्ट ग्रेन्यूलैरिटी या श्रृंखला में बांटता है।

```Swift
// इस उदाहरण में, हम एक हैं क्लासिक रूप से तारीख और समय मिलाते हैं और अनुलोमित हैं |
let date1 = Calendar.current.date(from: DateComponents(year: 2020, month: 1, day: 1, hour: 12, minute: 0))!
let date2 = Calendar.current.date(from: DateComponents(year: 2020, month: 1, day: 1, hour: 12, minute: 0))!

// कॉम्पेयर फंक्शन का उपयोग करके तारीखों को तुलना करें
let result1 = date1.compare(date2, toGranularity: .minute) // अपने आप से 0
let result2 = date1.compare(date2, toGranularity: .hour) // अपने आप से 0
let result3 = date1.compare(date2, toGranularity: .day) // अपने आप से 0

// < और > ऑपरेटर का उपयोग करके तारीखों को तुलना करें
let result4 = date1 < date2 // false
let result5 = date1 > date2 // false

// विशिष्ट ग