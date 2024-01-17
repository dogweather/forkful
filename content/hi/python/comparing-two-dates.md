---
title:                "दो तिथियों का तुलना करना"
html_title:           "Python: दो तिथियों का तुलना करना"
simple_title:         "दो तिथियों का तुलना करना"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/python/comparing-two-dates.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

किसी भी प्रोग्राम को बनाने के लिए हमें एक लम्बे समय को आयतन भाव के साथ दो तिथियों के बीच की तुलना करने की आवश्यकता पड़ती है। प्रोग्रामर इसको सही तरीके से करने के लिए तारीखों को तुलना करते हैं।

## कैसे करें?

```Python
# दिनांकों को संयोजित करने के लिए date और timedelta मॉड्यूल्स का उपयोग करें
from datetime import date, timedelta

# पहली तिथि
d1 = date(2021, 1, 1)

# दूसरी तिथि
d2 = date(2021, 1, 15)

# दो तिथियों के बीच की अंतराल के लिए तुलना करें
delta = d2 - d1

# उपादानों का प्रिंट करें
print("दूसरी तिथि बाद पहली तिथि " + str(delta.days) + " दिन है")
```

आउटपुट:

दूसरी तिथि बाद पहली तिथि 14 दिन है

## गाहना

प्यारे पाठकों, तुलना करने की ऐतिहासिक प्रणाली पहले से ही काफी पुरानी है। यह सबसे पहले रोमन तमिलाथान संख्याओं का प्रयोग मिथिल राज्य में हुआ था। यह सुलभ, तेज और आसान होने के कारण, देशीय विश्व में कई दशकों से इसका प्रयोग किया जाता है। अगर आपको पास समय नहीं है तो आप तुलनाओं में उपादानों के बीच अंतर के संग्रह से भी इस्तेमाल कर सकते हैं। चूँकिए, तुलना खुद का मुत्ली हैं, इसलिए आप उपादानों को उनके समय संकेतों के आधार पर तुलना कर सकते हैं।

## देखें

- [जीटीएक्स डेट ऑब्जेक्ट](https://docs.python.org/3/library/datetime.html#date-objects)
- [पाइथन में तारीख और समय](https://www.geeksforgeeks.org/python-dates/)
- [तुलना अंतराल की गणना पाइथन में](https://www.tutorialspoint.com/count-the-occurrences-of-the-difference-between-consecutive-numbers-by-using-python)