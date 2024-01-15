---
title:                "Bhavishya ya Bhoot ki tithi ka gina"
html_title:           "Python: Bhavishya ya Bhoot ki tithi ka gina"
simple_title:         "Bhavishya ya Bhoot ki tithi ka gina"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/python/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## क्यों

दिनांक को भविष्य या भूतकाल में गणना करने का काम है, वह व्यक्ति दो प्रमुख कारणों से कर सकता है। सबसे पहले, वह अपने कार्यों की योजना बनाने और उन्हें समय पर प्राप्त करने के लिए दिनांक की गणना कर सकता है। और दूसरे, वह इतिहास के लिए अलग-थलग दिनांकों को तुलना कर सकता है और उस से कुछ महत्वपूर्ण जानकारी हासिल कर सकता है।

## कैसे करें

```python
from datetime import date, timedelta

# वर्तमान दिनांक का प्राप्त करें
today = date.today()

# भविष्य में एक साल की गणना करें
# यहां फायदेमंद दिनांक है, जो साल के अंत में होगा
future_date = today + timedelta(days=365)

# पूर्ववर्ती दिनांक का उपयोग करें
# यहां गुजरे दिनांक की गणना की गई है, जो साल के शुरुआत में होगा
past_date = today - timedelta(days=365)

# गणना की गई दिनांकों को मान्य तारीख प्रारूप में प्रिंट करें
print("वर्तमान दिनांक:", today.strftime("%d/%m/%Y"))
print("भविष्य में दिनांक:", future_date.strftime("%d/%m/%Y"))
print("भूतकाल के दिनांक:", past_date.strftime("%d/%m/%Y"))
```

आउटपुट:
```
वर्तमान दिनांक: 25/08/2021
भविष्य में दिनांक: 25/08/2022
भूतकाल के दिनांक: 25/08/2020
```

## गहराई में जानें

दिनांक की गणना करने के लिए, हम `datetime` मॉड्यूल से तारीख और समय के आधार पर गणना करते हैं। `timedelta` ऑब्जेक्ट से हम दिनों, समय या मिलीसेकंड को आगे या पीछे जोड़ सकते हैं। आशा है कि यह लेख आपको द