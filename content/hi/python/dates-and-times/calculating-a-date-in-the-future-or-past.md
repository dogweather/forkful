---
date: 2024-01-20 17:32:27.500465-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: (How to:) \u0909\u0926\
  \u093E\u0939\u0930\u0923 \u0906\u0909\u091F\u092A\u0941\u091F."
lastmod: '2024-04-05T21:53:53.634561-06:00'
model: gpt-4-1106-preview
summary: "(How to:) \u0909\u0926\u093E\u0939\u0930\u0923 \u0906\u0909\u091F\u092A\u0941\
  \u091F."
title: "\u092D\u0935\u093F\u0937\u094D\u092F \u092F\u093E \u0905\u0924\u0940\u0924\
  \ \u092E\u0947\u0902 \u0924\u093E\u0930\u0940\u0916 \u0915\u0940 \u0917\u0923\u0928\
  \u093E"
weight: 26
---

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
