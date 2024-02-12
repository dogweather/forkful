---
title:                "तारीख को स्ट्रिंग में बदलना"
aliases:
- /hi/python/converting-a-date-into-a-string.md
date:                  2024-01-20T17:37:40.926870-07:00
model:                 gpt-4-1106-preview
simple_title:         "तारीख को स्ट्रिंग में बदलना"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/python/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
तारीख को स्ट्रिंग में बदलना यानी एक ऐसी प्रक्रिया है जिसमें तारीख को पठनीय टेक्स्ट फॉर्म में परिवर्तित किया जाता है। प्रोग्रामर्स इसका इस्तेमाल लॉग्स, यूजर इंटरफेस और डेटा स्टोरिंग के लिए करते हैं।

## कैसे करें:
```Python
from datetime import datetime

# वर्तमान तारीख और समय प्राप्त करें
current_datetime = datetime.now()

# तारीख को स्ट्रिंग में परिवर्तित करें
date_string = current_datetime.strftime('%d-%m-%Y %H:%M:%S')

# परिवर्तित स्ट्रिंग दिखाएँ
print(date_string)
```
सैंपल आउटपुट:
```
05-04-2023 21:45:12
```

## गहराई से जानकारी:
जब सी प्रोग्रामिंग भाषा विकसित हुई थी, तब से ही `printf` जैसे फंक्शंस के द्वारा डेटा को स्ट्रिंग में परिवर्तित करने की क्षमता थी। Python में, `datetime` मॉड्यूल `strftime` मेथड के साथ यह कार्य बहुत ही सुविधाजनक बना देता है। विकल्प के रूप में, आप `isoformat()` या फिर `str()` फंक्शन का इस्तेमाल कर सकते हैं, परंतु `strftime` आपको कस्टम फॉर्मेट प्रदान करता है। आम तौर पर, हम डेट और टाइम को `%d`, `%m`, `%Y`, `%H`, `%M`, `%S` जैसे डायरेक्टिव्स के साथ फॉर्मेट करते हैं।

## इसे भी देखें:
- Python ऑफिशियल डॉक्स का `datetime` मॉड्यूल: https://docs.python.org/3/library/datetime.html
- `strftime()` और `strptime()` के लिए डायरेक्टिव्स की गाइड: https://strftime.org/
