---
title:                "वर्तमान तारीख प्राप्त करना"
date:                  2024-01-20T15:16:38.015023-07:00
html_title:           "C: वर्तमान तारीख प्राप्त करना"
simple_title:         "वर्तमान तारीख प्राप्त करना"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/python/getting-the-current-date.md"
---

{{< edit_this_page >}}

## क्या और क्यों? (What & Why?)
वर्तमान तारीख प्राप्त करना यानी आज की तारीख जानना होता है। प्रोग्रामर्स ये इसलिए करते हैं ताकि वे ऐप्लिकेशन्स में लॉगिंग, टाइमस्टैम्प्स या वैधता परीक्षण कर सकें।

## कैसे करें? (How to:)
उदाहरण:
```Python
from datetime import date

# वर्तमान तारीख प्राप्त करें
aaj_ki_taarikh = date.today()

# तारीख को स्ट्रिंग के रूप में प्रिंट करें
print(f"आज की तारीख है: {aaj_ki_taarikh}")
```
सैंपल आउटपुट:
```
आज की तारीख है: 2023-04-14
```

## गहराई से जानकारी (Deep Dive)
डेटटाइम मॉड्यूल का प्रयोग करके Python में वर्तमान तारीख प्राप्त करना आसान है, जो 1995 में जारी हुआ था। इसके विकल्प के रूप में time मॉड्यूल भी है, पर डेटटाइम में ज्यादा सुविधाएँ हैं। `date.today()` के पीछे कार्यान्वयन सिस्टम क्लॉक का उपयोग करता है और टाइमज़ोन निरपेक्ष (timezone-naive) तारीख देता है। यदि समय क्षेत्र समर्थित तारीख की आवश्यकता हो तो pytz जैसे थर्ड-पार्टी लाइब्रेरीज का इस्तेमाल करना पड़ता है।

## यह भी देखें (See Also)
- Python की आधिकारिक डॉक्यूमेंटेशन: https://docs.python.org/3/library/datetime.html
- pytz लाइब्रेरी: https://pypi.org/project/pytz/
- time मॉड्यूल डॉक्यूमेंटेशन: https://docs.python.org/3/library/time.html
