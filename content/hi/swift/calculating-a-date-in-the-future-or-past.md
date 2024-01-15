---
title:                "भविष्य या भूत की तारीख की गणना"
html_title:           "Swift: भविष्य या भूत की तारीख की गणना"
simple_title:         "भविष्य या भूत की तारीख की गणना"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## आखिर क्यों

अगर आप को लगता है कि आपको भविष्य में किसी तारीख को पता होना है या जानना है कि कितने दिन पहले या बाद कितने दिनों आगे तक तारीख है, तो आपको आज सीखना चाहिए कि स्विफ्ट प्रोग्रामिंग में इसे कैसे करते हैं।

## कैसे करें

अगर आपको किसी विशेष दिन की तारीख को पता करना है, तो मेरे पास कुछ स्विफ्ट कोड उदाहरण है जो आपको इस प्रकार का काम करने में मदद कर सकते हैं:

```Swift
// अगले साल की आज की तारीख को लेना
let currentDate = Date()
var dateComponent = DateComponents()
dateComponent.year = 1
let nextYear = Calendar.current.date(byAdding: dateComponent, to: currentDate)

// पिछले साल की आज की तारीख को लेना
let previousYear = Calendar.current.date(byAdding: .year, value: -1, to: currentDate)

// आज से 30 दिन आगे तारीख को लेना
dateComponent = DateComponents()
dateComponent.day = 30
let thirtyDaysFromNow = Calendar.current.date(byAdding: dateComponent, to: currentDate)

// आज से 30 दिन पहले तारीख को लेना
let thirtyDaysAgo = Calendar.current.date(byAdding: .day, value: -30, to: currentDate)
```

इस तरह से आप स्विफ्ट के क्लास और तारीख तथा समय के साथ मिलकर आसानी से भविष्य में या अतीत में तारीख को पता कर सकते हैं।

## डीप डाइव

जब हम स्विफ्ट में तारीख को कैल्कुलेट करते हैं, तो हम अक्सर `Date` और `DateComponents` किंतु क्लास का इस्तेमाल करते हैं। `Date` एक स्पष्ट तारीख औऱ समय को दर्शाते होता है, जबकि `DateComponents` एक निर्दिष्ट तारीख के तत्वों को नया तारीख बनाने के लिए इस्तेमाल होता है। आप `DateComponents` के माध्यम से विभिन्न तारीखीय यूनिट जैसे दिन, महीने,