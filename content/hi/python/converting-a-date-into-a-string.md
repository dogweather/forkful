---
title:                "एक तारीख को स्ट्रिंग में परिवर्तित करना"
html_title:           "Java: एक तारीख को स्ट्रिंग में परिवर्तित करना"
simple_title:         "एक तारीख को स्ट्रिंग में परिवर्तित करना"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/python/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

डेट को स्ट्रिंग में बदलना एक ऐसा प्रक्रिया है जिसमें हम डेट's को human-readable text format में बदलते हैं। प्रोग्रामर्स इसे tabular form या फिर output को समुचित रूप से प्रस्तुत करने के लिए करते हैं।

## कैसे करें:

एक Python code के माध्यम से हम डेट को स्ट्रिंग में कैसे बदल सकते हैं, उसे समझें:

```Python
from datetime import date

# आज की तारीख प्राप्त करें
today = date.today()

# तारीख को स्ट्रिंग में कन्वर्ट करें
date_string = today.strftime("%d/%m/%Y")

print(date_string)  # प्रिंट करें
```
Sample Output:
```Python
'27/05/2022'
```
## गहरा गोता

स्ट्रिंग में डेट कन्वर्शन की आवश्यकता उन समयों में होती है, जब हमें एक तारीख को उपयोगकर्ता को सोचे बिना पढ़ने योग्य रूप में प्रदर्शित करना हो। `%d`, `%m`, और `%Y` हमारे दिन, महीने, और वर्ष की formatting पुरण के सबसे सामान्य parameters हैं।

## अन्य जानकारी रसिद:

आप यहां अधिक जानकारी प्राप्त करने के लिए देख सकते हैं: 
- Python के official documentation पर [datetime](https://docs.python.org/3/library/datetime.html)
- Python के official documentation पर [strftime](https://docs.python.org/3/library/datetime.html#strftime-and-strptime-behavior)