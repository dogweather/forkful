---
title:                "भविष्य या अतीत में तारीख की गणना"
aliases: - /hi/python/calculating-a-date-in-the-future-or-past.md
date:                  2024-01-20T17:32:27.500465-07:00
model:                 gpt-4-1106-preview
simple_title:         "भविष्य या अतीत में तारीख की गणना"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/python/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## क्या और क्यों? (What & Why?)
तारीख को भविष्य या अतीत में गणना करना मतलब है किसी निश्चित दिनांक से पहले या बाद की तारीख जानना। प्रोग्रामर इसे बिलिंग सिस्टम्स, इवेंट प्लानिंग, या रिमाइंडर टूल्स में इस्तेमाल करते हैं।

## कैसे करें: (How to:)
```Python
from datetime import datetime, timedelta

# वर्तमान तारीख प्राप्त करना
current_date = datetime.now()

# भविष्य में 10 दिन जोड़ना
future_date = current_date + timedelta(days=10)

# अतीत में 10 दिन घटाना
past_date = current_date - timedelta(days=10)

print("आज की तारीख:", current_date.strftime('%Y-%m-%d'))
print("भविष्य का दिन:", future_date.strftime('%Y-%m-%d'))
print("अतीत का दिन:", past_date.strftime('%Y-%m-%d'))
```
उदाहरण आउटपुट:
```
आज की तारीख: 2023-04-01
भविष्य का दिन: 2023-04-11
अतीत का दिन: 2023-03-22
```

## गहराई से जानकारी (Deep Dive)
तारीख की गणना में समय-प्रबंधन की भूमिका महत्वपूर्ण है। पुराने समय में, कैलेंडर और दिवाली चार्ट के जरिए इसे हाथ से किया जाता था। अब पायथन में `datetime` और `timedelta` के उपयोग से यह काम आसानी से हो जाता है। `datetime` वर्तमान तारीख और समय देता है, जबकि `timedelta` उसमें निश्चित समय जोड़ने या घटाने का काम करता है।

विकल्प रूप में, `dateutil` लाइब्रेरी भी मौजूद है जो और भी जटिल तारीख गणना कर सकता है। प्रोग्रामिंग में तारीख की गणना का सटीक होना जरूरी है क्योंकि लीप इयर्स, टाइम जोन्स, और डेलाइट सेविंग्स से जुड़ी जटिलताएं होती हैं।

## संबंधित स्रोत (See Also)
- Python `datetime` मॉड्यूल का डॉक्युमेंटेशन: https://docs.python.org/3/library/datetime.html
- `dateutil` लाइब्रेरी: https://dateutil.readthedocs.io/en/stable/
- पायथन में समय संबंधी गणना विश्लेषण: https://pymotw.com/3/datetime/
