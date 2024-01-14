---
title:                "Python: तारीख को स्ट्रिंग में रूपांतरण करना"
programming_language: "Python"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/python/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## क्यों
JavaScript आदि बहुत से प्रोग्रामिंग भाषाओं में, तारीखों को स्ट्रिंग में बदलने की जरूरत होती है। आप ऐसा क्यों करना चाहते हैं?

## कैसे करें
```
# तारीख और समय ऑब्जेक्ट बनाएं
import datetime
dt = datetime.datetime(2020, 10, 30, 9, 45)

# तारीख को स्ट्रिंग में बदलें
date_string = dt.strftime("%d-%m-%Y %H:%M:%S")
print(date_string) # Output: 30-10-2020 09:45:00
```
सरल शब्दों में, हमने `strftime()` फंक्शन का उपयोग करके तारीख को स्ट्रिंग में बदला है। यह स्ट्रिंग फॉर्मेटिंग के लिए उपयोगी है जहाँ हम अपनी इच्छानुसार दिनांक, समय और समय प्रभाव का चयन कर सकते हैं।

## गहराई में जाएं
स्ट्रिंग में तारीख मानों के प्रबंधन ने आपको अपनी उपस्थिति का प्रदर्शन करने का एक आकर्षक विकल्प दिया है। इसके अलावा, इसका उपयोग समूहित सांख्यिकी और अन्य डेटा विश्लेषण के लिए भी किया जा सकता है।

## देखें भी
- [Python तारीख और समय](https://docs.python.org/3/library/datetime.html)
- [मॉड्यूल स्ट्रिंग फ़ॉर्मेटिंग](https://www.w3schools.com/python/python_datetime.asp)
- [तारीखों के साथ काम करना](https://realpython.com/python-datetime/)