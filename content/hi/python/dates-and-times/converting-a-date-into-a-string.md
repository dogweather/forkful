---
changelog:
- 2024-04-04, dogweather, edited
- 2024-04-04, gpt-4-0125-preview, translated from English
date: 2024-01-20 17:37:21.660960-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: Python \u0924\u093E\
  \u0930\u0940\u0916\u094B\u0902 \u0915\u094B \u0938\u094D\u091F\u094D\u0930\u093F\
  \u0902\u0917 \u092E\u0947\u0902 \u092C\u0926\u0932\u0928\u093E \u0906\u0938\u093E\
  \u0928 \u092C\u0928\u093E\u0924\u093E \u0939\u0948\u0964 [\u0926\u093F\u0928\u093E\
  \u0902\u0915](https://docs.python.org/3/library/datetime.html#date-objects) \u0935\
  \u0938\u094D\u0924\u0941\u0913\u0902 \u092A\u0930 \u0909\u092A\u0932\u092C\u094D\
  \u0927\u2026"
lastmod: '2024-04-04T02:03:09.409416-06:00'
model: gpt-4-0125-preview
summary: "Python \u0924\u093E\u0930\u0940\u0916\u094B\u0902 \u0915\u094B \u0938\u094D\
  \u091F\u094D\u0930\u093F\u0902\u0917 \u092E\u0947\u0902 \u092C\u0926\u0932\u0928\
  \u093E \u0906\u0938\u093E\u0928 \u092C\u0928\u093E\u0924\u093E \u0939\u0948\u0964\
  \ [\u0926\u093F\u0928\u093E\u0902\u0915](https://docs.python.org/3/library/datetime.html#date-objects)\
  \ \u0935\u0938\u094D\u0924\u0941\u0913\u0902 \u092A\u0930 \u0909\u092A\u0932\u092C\
  \u094D\u0927 [`strftime`](https://docs.python.org/3/library/datetime.html#strftime-and-strptime-behavior)\
  \ \u0935\u093F\u0927\u093F \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\
  \u0947\u0902\u0964 \u0910\u0938\u0947."
title: "\u090F\u0915 \u0924\u093E\u0930\u0940\u0916 \u0915\u094B \u0938\u094D\u091F\
  \u094D\u0930\u093F\u0902\u0917 \u092E\u0947\u0902 \u0915\u0928\u094D\u0935\u0930\
  \u094D\u091F \u0915\u0930\u0928\u093E"
weight: 28
---

## कैसे करें:
Python तारीखों को स्ट्रिंग में बदलना आसान बनाता है। [दिनांक](https://docs.python.org/3/library/datetime.html#date-objects) वस्तुओं पर उपलब्ध [`strftime`](https://docs.python.org/3/library/datetime.html#strftime-and-strptime-behavior) विधि का उपयोग करें। ऐसे:

```Python
from datetime import datetime

# वर्तमान दिनांक और समय प्राप्त करें
now = datetime.now()

# इसे इस प्रारूप में एक स्ट्रिंग में बदलें: महीना दिन, वर्ष
date_string = now.strftime("%B %d, %Y")
print(date_string)  # परिणाम: मार्च 29, 2023 (या वर्तमान दिनांक)

# प्रारूप: YYYY-MM-DD
iso_date_string = now.strftime("%Y-%m-%d")
print(iso_date_string)  # परिणाम: 2023-03-29 (या वर्तमान दिनांक)
```


### मैं इसे कैसे करता हूं

यह है कि मैं टाइमज़ोन जानकारी के साथ एक [ISO 8601](https://www.w3.org/QA/Tips/iso-date) प्रारूप दिनांक कैसे प्राप्त करता हूं:

```python
def datestamp() -> str:
    """ 
    टाइमज़ोन के साथ ISO प्रारूप में वर्तमान दिनांक और समय।
    """
    return datetime.now().astimezone().isoformat()
```

#### उदाहरण परिणाम:

```python
>>> datestamp()
'2024-04-04T01:50:04.169159-06:00'
```



## गहराई से समझें
ऐतिहासिक रूप से, प्रोग्रामिंग में तारीख-स्ट्रिंग रूपांतरण मानव पठनीय प्रारूप में तारीखों को प्रस्तुत करने की आवश्यकता के कारण एक स्टेपल रहा है। 

`strftime` के विकल्पों में ISO 8601 प्रारूप के लिए `isoformat` विधि या `arrow` और `dateutil` जैसे तृतीय-पक्ष पुस्तकालयों का उपयोग शामिल है जो अधिक लचीले पार्सिंग और प्रारूपण विकल्प प्रदान करते हैं।

कार्यान्वयन-वार, `strftime` का अर्थ "स्ट्रिंग प्रारूप समय" है और इसकी जड़ें C प्रोग्रामिंग में हैं। Python का `strftime` वर्ष के लिए `%Y` और महीने के लिए `%m` जैसे प्रारूप कोडों की व्याख्या करता है, जिससे लगभग अंतहीन अनुकूलन संभव हो जाता है।

## और देखें
Python के दिनांक और समय कार्यों में गहराई से डुबकी लगाने के लिए:
- Python की आधिकारिक `datetime` दस्तावेज़ीकरण: https://docs.python.org/3/library/datetime.html
- `strftime` निर्देशांकों की एक व्यापक सूची में रुचि रखने वालों के लिए: https://strftime.org/
- तृतीय-पक्ष दिनांक/समय पुस्तकालयों का अन्वेषण करने के लिए:
  - Arrow: https://arrow.readthedocs.io/en/latest/
  - python-dateutil: https://dateutil.readthedocs.io/en/stable/
