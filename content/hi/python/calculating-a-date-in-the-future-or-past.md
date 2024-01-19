---
title:                "भविष्य या अतीत में तारीख की गणना"
html_title:           "Python: भविष्य या अतीत में तारीख की गणना"
simple_title:         "भविष्य या अतीत में तारीख की गणना"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/python/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
भविष्यधिक्कर या अतीत में एक तारीख की गणना सॉफ्टवेयर में कमन्ली उपयोगिता की एक प्रक्रिया है। प्रोग्रामर्स इसे समय-संबंधी कार्यवाही, माहिती छुपावने, और तारीखों के बीच के विभेद की गणना के लिए करते हैं। 

## कैसे:
```Python
from datetime import datetime, timedelta

# आज की तारीख
today = datetime.now()

# 20 दिन भविष्य में की तारीख
future_date = today + timedelta(days=20)
print('Future Date: ', future_date)

# 20 दिन अतीत में की तारीख
past_date = today - timedelta(days=20)
print('Past Date: ', past_date)
```
ऊपर दिए गए कोड का आउटपुट दिए गए समय के आधार पर होगा।

## गहराई से:
तारीख की गणना समय के साथ बदलती रहती है, लेकिन पायथन में `datetime` और `timedelta` फंक्शन का उपयोग करके यह आसानी से संभव होता है। वैकल्पिक तरीकों में आप python के `dateutil` लाइब्रेरी का उपयोग कर सकते हैं। लेकिन `timedelta` स्थापित पायथन के हिस्से का एक बहुत ही व्यापक, व्यापक और तात्विक उपयोग है।

## देखें भी:
1. [Python Docs: Datetime](https://docs.python.org/3/library/datetime.html) - Python दस्तावेज़ों में डेटाइम मॉड्यूल से संबंधित अधिक जानकारी।
2. [Dateutil Docs](https://dateutil.readthedocs.io/en/stable/) - डेटाइम मॉड्यूल के वैकल्पिक पुस्तकालय के बारे में अधिक जानकारी।
3. [Time complexity of Python operations](https://wiki.python.org/moin/TimeComplexity) - विभिन्न पायथन ऑपरेशन के समयों की जटिलता के बारे में अधिक जानकारी।
4. [Python Tutor](http://www.pythontutor.com/) - ऑनलाइन विजुअलेशन उपकरण जो पायथन कोड चलाता है।