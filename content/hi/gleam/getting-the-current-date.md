---
title:                "वर्तमान दिनांक प्राप्त करना"
html_title:           "Gleam: वर्तमान दिनांक प्राप्त करना"
simple_title:         "वर्तमान दिनांक प्राप्त करना"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/gleam/getting-the-current-date.md"
---

{{< edit_this_page >}}

## क्यों
वर्तमान में तारीख प्राप्त करने के लिए क्यों कोई आरंभ करेगा? वंश: वर्तमान पत्रक
के अनुसार आप किसी भी कोड में वर्तमान तारीख को प्राप्त कर सकते हैं जो आपको कंप्यूटर प्रोग्राम के समय को पहचानने में मदद कर सकता है।

## कैसे करें
कोडिंग उदाहरण और "Gleam ... " कोड ब्लॉक के साथ नमूना आउटपुट दिखाएं।
```Gleam
import Gleam.Date
let current_date = Date.Local.today()
```
आउटपुट: `#Gleam.Date.Local<2022, 1, 22>`

## गहरा परीक्षण
V#गहरा एम्बेडेड टाइमजोन है। इसके साथ आप वर्तमान तारीख के साथ अन्य जानकारी प्राप्त कर सकते हैं जैसे वर्तमान समय या UTC का समय।
```Gleam
import Gleam.Date
let current_date = Date.Local.today()
let current_time = Date.Local.now()
let UTC_date = Date.UTC.today()
let UTC_time = Date.UTC.now()
```
आउटपुट:
```Gleam
#current_date: Gleam.Date.Local<2022, 1, 22>
#current_time: Time.Local<10, 24, 0, 31_435_000_000>
#UTC_date: Gleam.Date.UTC<2022, 1, 22>
#UTC_time: Time.UTC<15, 54, 0, 125_305_000_000>
```

## गहरी जाँच
Gleam में आप इस तरह से भी वर्तमान तारीख को प्राप्त कर सकते हैं:
```Gleam
import Gleam.Date

let current_date = Date.from_year_month_day(2022, 1, 22)
```
आउटपुट:`#Gleam.Date.Local<2022, 1, 22>`

## देखिए भी
- [Gleam डॉक्यूमेंटेशन](https://gleam.run/documentation/)
- [Gleam ट्यूटोरियल](https://gleam.run/tutorials/)