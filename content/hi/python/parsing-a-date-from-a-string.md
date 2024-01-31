---
title:                "स्ट्रिंग से दिनांक पार्स करना"
date:                  2024-01-20T15:38:36.410318-07:00
html_title:           "Arduino: स्ट्रिंग से दिनांक पार्स करना"
simple_title:         "स्ट्रिंग से दिनांक पार्स करना"

category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/python/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों? (What & Why?)
डेट को स्ट्रिंग से पार्स करना मतलब है टेक्स्ट के रूप में आया डेटा को पायथन की डेटटाइम ऑब्जेक्ट में बदलना। इससे डेटाबेस में स्टोर करना, सॉर्ट करना या कंडीशनल ऑपरेशंस करना आसान हो जाता है।

## कैसे करें? (How to:)
```Python
from datetime import datetime

# स्ट्रिंग से डेट पार्स करने का उदाहरण:
date_string = "25-03-2023"
date_object = datetime.strptime(date_string, "%d-%m-%Y")
print(date_object)  # 2023-03-25 00:00:00

# अलग-अलग फॉरमेट में डेट पार्स करना:
date_string_2 = "March 25, 2023, 14:30"
date_object_2 = datetime.strptime(date_string_2, "%B %d, %Y, %H:%M")
print(date_object_2)  # 2023-03-25 14:30:00
```

## गहरी जानकारी (Deep Dive)
डेट पार्सिंग 1970 के दशक से प्रोग्रामिंग में आम है, जब से कंप्यूटर्स में डेट्स का इस्तेमाल बढ़ा। `strptime()` मेथड, जो C लैंग्वेज से आया है, का इस्तेमाल कर के हम स्ट्रिंग से डेट ऑब्जेक्ट्स बनाते हैं। विकल्पों में `dateutil.parser` शामिल है जो कई फॉरमेट्स को फ्लेक्सिबली हैंडल करता है। पर `datetime.strptime()` सीधा और तेज है अगर फॉरमेट पता हो तो।

## संबंधित सूत्रों के लिंक्स (See Also)
- `datetime` मॉड्यूल का ऑफिशियल डॉक्युमेंटेशन: https://docs.python.org/3/library/datetime.html
- `dateutil.parser` का डॉक्युमेंटेशन: https://dateutil.readthedocs.io/en/stable/parser.html
- डेट और टाइम पार्सिंग के बेस्ट प्रैक्टिसेस: https://realpython.com/python-datetime/
