---
title:                "एक स्ट्रिंग से तारीख पार्स करना"
html_title:           "C++: एक स्ट्रिंग से तारीख पार्स करना"
simple_title:         "एक स्ट्रिंग से तारीख पार्स करना"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/python/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## क्या & क्यों?

दिनांकों को स्ट्रिंग से पार्स करना मतलब होता है दिनांकों को स्ट्रिंग से ढूंढना और उसे प्यथन दिनांक वस्तु में बदल देना। प्रोग्रामर्स इसे उन दिनांकों को संदर्भित करने और उन्हें मान्यतानुसार संचालित करने के लिए करते हैं, जो स्ट्रिंग के रूप में प्रदान की गई हो।

## कैसे करें:

विभिन्न तरीकों से Python में दिनांकों को स्ट्रिंग से पार्स कर सकते हैं। एक आम तरीका `datetime.strptime` का उपयोग करना है।

```Python
from datetime import datetime

# आपकी तारीख जो आप पार्स करना चाहते हैं
date_string = "21 June, 2018"

# दिनांक को पार्स करें
date_object = datetime.strptime(date_string, "%d %B, %Y")

print(date_object)
```

```
2018-06-21 00:00:00
```

## गहराई से जानकारी:

1. ऐतिहासिक प्रसंग: Python में `strptime` मेथड की उत्पत्ति C’s `strptime()` फ़ंक्शन से हुई है, जो एक स्ट्रिंग से दिनांक पार्स करता था।
2. विकल्प: अन्य जबरजस्त लाइब्ररी पायथन `dateutil` है जो एक स्वतंत्र पाठ पार्सर(`parser.parse`) प्रदान करती है।
3. कार्यान्व्यान विवरण: `strptime` मेथड का काम पाठ मैपिंग को पार्स करने और दिनांक ऑब्जेक्ट में परिवर्तन करने का है।

## देखें भी:

- Python दस्तावेज़ीकरण: https://docs.python.org/3/library/datetime.html
- dateutil लाइब्ररी: https://dateutil.readthedocs.io/en/stable/parser.html
- Python के `strptime` और `strftime` फ़ॉर्मेट नियम: http://strftime.org/