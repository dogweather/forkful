---
title:                "तारीख को स्ट्रिंग में रूपांतरित करना"
html_title:           "Python: तारीख को स्ट्रिंग में रूपांतरित करना"
simple_title:         "तारीख को स्ट्रिंग में रूपांतरित करना"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/python/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

जब हम कोई रोजमर्रा की तारीख (date) को पाठ (string) में बदलते हैं, तो हम उसे "date string" बनाते हैं। यह एक आम क्रिया है जो प्रोग्रामर्स द्वारा उनके कोड में ऊपरी स्तर का आकार दर्शाने के लिए किया जाता है।

## कैसे करें?

```python
# साल, महीना और दिन को date string में बदलते हैं
year = 2020
month = 10
day = 27
date_string = f"{month}/{day}/{year}"
print(date_string) # 10/27/2020

# strftime() का उपयोग करके date string बनाना
import datetime
today = datetime.datetime.today()
print(today.strftime("%m/%d/%Y")) # 10/27/2020

# उल्लेखनीय तारीखों को custom रूप में प्रिंट करना
import datetime
birth_year = 1995
birth_month = 5
birth_date = 16
birthday = datetime.datetime(birth_year, birth_month, birth_date)
print(birthday.strftime("मेरा जन्मदिन: %d %B %Y")) # मेरा जन्मदिन: 16 मई 1995
```

## गहराईगमन

- एक प्राचीन प्रोग्रामिंग संस्कृति में, तारीख और समय को संख्याओं के रूप में प्रस्तुत किया जाता था जिससे पढ़ना और समझना काफी मुश्किल था। date string का आविष्कार ने प्रोग्रामिंग को सुविधाजनक बनाया।
- date string बनाने के अलावा, अन्य दस्तावेजों को भी date string में रूपांतरित किया जाता है। उदाहरण के लिए, ऑनलाइन पंजीकरण फॉर्म या समय के साथ शैक्षिक यात्राओं का कार्यक्रम।
- date string को बनाने के लिए अन्य प्रोग्रामिंग भाषाओं में भी लाइब्रेरी (library) उपलब्ध है। उदाहरण के लिए, PHP में date() और C# में ToString() का उपयोग किया जाता है।

## इससे जुड़ी लिंकें

- [Python documentation on date string formatting](https://docs.python.org/3/library/datetime.html#strftime-and-strptime-format-codes)
- [Python tutorial on converting a date into a string](https://www.programiz.com/python-programming/datetime/string)
- [StackOverflow discussion on date string conversion in Python](https://stackoverflow.com/questions/7992935/int-to-date-objects)