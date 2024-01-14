---
title:                "Swift: भविष्य या भूतकाल में तारीख की गणना"
simple_title:         "भविष्य या भूतकाल में तारीख की गणना"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## क्यों

एक दिनांक उसकी तारीख को गणना करने को व्यक्ति के लिए सिर्फ दस्तावेज़ीकरण और समर्थन का उपयोग कर सकता है।

## कैसे करें

```Swift
let today = Date()
let tomorrow = Calendar.current.date(byAdding: .day, value: 1, to: today)
print(tomorrow)
```

उपरोक्त कोड लाइन आपको मान से एक दिन बाद की तारीख निकालेगी। सम्पूर्ण कोड रन गोद भविष्य एवं भूतकाल के साथ साथ भावी दिनांकों को भी निकाल सकते हैं।

```
let yesterday = Calendar.current.date(byAdding: .day, value: -1, to: today)
print(yesterday)
let nextWeek = Calendar.current.date(byAdding: .week, value: 1, to: today)
print(nextWeek)
```

उपरोक्त कोड लाइन आपको आलेख की तारीख से एक दिन पहले की तारीख प्रिंट करेगी, उसके बाद आने वाली सप्ताह की भावी दिनांक प्रिंट करेगी।

## गहराई का निरीक्षण

गहराई का निरीक्षण करने के लिए, हमें यहां NSDate स्ट्रक्चर का उपयोग करना चाहिए। इससे भूतकाल का के साथ साथ भविष्य की तारीख भी निकाल सकते हैं। हालांकि, हमें एक महीने से भी अधिक समय की तारीख गणना करते समय ध्यान देनी पड़ती है क्योंकि यह अक्सर द्वारा समस्या उत्पन्न होती है।

## देखें भी

तपासें कर सकते हैं यह पद यदि आप भविष्य की तारीखों की गणना से जुड़े और उड़्धरणों की खोज कर रहे हैं। 

- [NSDate स्ट्रक्चर के साथ तारीखों की गणना (Swift)](https://stackoverflow.com/a/3307837)
- [भावी दिनांकों की गणना उन्नततम समय-साध्य के साथ (हिंदी)](https://techforum4u.com/mathematics/finding-the-future-dates-easily/)