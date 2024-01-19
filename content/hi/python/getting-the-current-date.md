---
title:                "वर्तमान तारीख प्राप्त करना"
html_title:           "C#: वर्तमान तारीख प्राप्त करना"
simple_title:         "वर्तमान तारीख प्राप्त करना"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/python/getting-the-current-date.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

वर्तमान तारीख प्राप्त करना इसका मतलब है कि हम अभी कौन सी तारीख है, यह जानने की कोशिश कर रहे हैं। कार्यक्रमकर्ता इसे तब करते हैं जब हमें लॉग फ़ाइलों, समय-मिटाव के फ़ंक्शन या स्थितिसूचकों को मार्क करने की आवश्यकता हो। 

## कैसे करें:

Python में, हम वर्तमान तारीख को `datetime` मॉड्यूल के `date.today()` फ़ंक्शन का उपयोग करके प्राप्त कर सकते हैं।

```Python
from datetime import date

# वर्तमान तारीख प्राप्त करें
today = date.today()
print("आज की तारीख है:", today)
```

जब आप ऊपरी कोड चलाएंगे, तो आपको मिलेगा:

```Python
'आज की तारीख है: 2022-09-15'
```

जो आपकी वर्तमान तारीख होगी।

## गहराई में:

1. `datetime` मॉड्यूल Python के स्टैंडर्ड लाइब्रेरी का हिस्सा है एवं 1990 के 1990 में जन्मे Python से ही मौजूद है।
2. वैकल्पिक रूप से, आप `time` मॉड्यूल का उपयोग कर के भी वर्तमान समय प्राप्त कर सकते हैं, लेकिन `datetime` आमतौर पर काम करने के लिए अधिक व्यापक और सुविधाजनक होता है।
3. `date.today()` फ़ंक्शन "होम" टाइमज़ोन में वर्तमान तारीख को देता है। यदि आपको वर्तमान समय में सर्वश्रेष्ठ क्षेत्र की आवश्यकता है, तो `datetime.utcnow()` का उपयोग करें।

## यदि आप देखना चाहते हैं:

1. [Python की आधिकारिक दस्तावेज़ीकरण `datetime`](https://docs.python.org/3/library/datetime.html)
2. [Python की आधिकारिक `time` दस्तावेज़ीकरण](https://docs.python.org/3/library/time.html)
3. [Python में दिनांक और समय का उपयोग](https://realpython.com/python-datetime/) वाले स्मरणपत्र पर Real Python.