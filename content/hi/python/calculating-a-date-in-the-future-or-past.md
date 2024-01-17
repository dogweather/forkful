---
title:                "भविष्य या भूतकाल में एक तारीख का गणना"
html_title:           "Python: भविष्य या भूतकाल में एक तारीख का गणना"
simple_title:         "भविष्य या भूतकाल में एक तारीख का गणना"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/python/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
किसी भी दिनांक को भविष्य या भूतकाल में परिकलित करने को गणना करना है। प्रोग्रामर्स इसलिए करते हैं कि वे निश्चित दिनांकों पर नक्शाबंदी अथवा अवधि को जांचना चाहते हैं।

## कैसे करें:
```Python
import datetime
today = datetime.date.today()
future = today + datetime.timedelta(days=10)
print(future)
```
Output:
```2021-06-29```

## गहराई में जाएँ:
(1) इतिहास के संदर्भ में। (2) विकल्प। (3) भविष्य में या पूर्व में दिनांक की गणना करने के लिए कार्यान्वयन विवरण।

## इससे भी देखें:
रिकमेंडेड स्रोतों के लिए लिंक।