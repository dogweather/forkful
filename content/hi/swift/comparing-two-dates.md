---
title:                "Swift: दो तिथियों की तुलना करना"
simple_title:         "दो तिथियों की तुलना करना"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/comparing-two-dates.md"
---

{{< edit_this_page >}}

## क्यों
कैसे दो तारीखों की तुलना करने से कोई लाभ हो सकता है? यह आमतौर पर बहुत सारे उदाहरण में उपयोगी हो सकता है, जैसे कि बैंकिंग ट्रांजैक्शन, और ऐसे समय की ज़रूरत हो सकती है जब हमें दो तारीखों को तुलना करने की ज़रूरत होती है।

## कैसे करें
तारीखों की तुलना करने के लिए हम `Calendar` का उपयोग कर सकते हैं जो निम्न तरह से दो तारीखों के बीच की दूरी की गणना करता है:

```Swift
let calendar = Calendar.current
let startDate = calendar.startOfDay(for: Date())
let endDate = calendar.date(byAdding: .day, value: 7, to: startDate)!

let differenceInDays = calendar.dateComponents([.day], from: startDate, to: endDate).day
print(differenceInDays!) // Output: 7
```

इस उदाहरण में हम स्थानीय टारीख को प्राप्त करने के लिए `startOfDay()` फंक्शन का उपयोग करते हैं, उसके बाद हम 7 दिन को शामिल करने के लिए `date(byAdding:to:)` फंक्शन का उपयोग करते हैं। फिर हम शुरुआत तारीख और अंत तारीख के बीच की दिनों की गणना करने के लिए `dateComponents(_:from:to:)` फंक्शन का उपयोग करते हैं और उसकी दिन की वैल्यू को प्रिंट करते हैं।

## डीप डाइव
तुलना करने के लिए दो तारीखों के बीच की दुर्लभता को समझने के लिए, हमें `DateInterval` डेटा संरचना का उपयोग करना चाहिए। इस तरह, हम एक अधिक आरामदायक और सुलभ तरीके से तारीखों की तुलना कर सकते हैं। इस तरह से, हम किसी भी अंतराल को सुलभता से बूट करके तत्काल जान सकते हैं कि क्या दो तारीखें समान हैं या