---
title:                "यादृच्छिक संख्याएँ उत्पन्न करना"
date:                  2024-01-20T17:50:11.438147-07:00
model:                 gpt-4-1106-preview
simple_title:         "यादृच्छिक संख्याएँ उत्पन्न करना"
programming_language: "Python"
category:             "Python"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/python/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
रैंडम नंबर्स वे होते हैं जो कोई पैटर्न नहीं दिखाते। प्रोग्रामर्स इन्हें टेस्टिंग, सिमुलेशन, और सिक्योरिटी में इस्तेमाल करते हैं।

## How to (कैसे करें):
Python में रैंडम नंबर्स जेनरेट करना सीधा है:

```Python
import random

# रैंडम इंटीजर 1 से 10
random_integer = random.randint(1, 10)
print(random_integer)

# रैंडम फ्लोट 0 से 1 के बीच
random_float = random.random()
print(random_float)
```

Sample Output:
```
4
0.935559507173
```

ऐसे कोड से आपको हर बार एक नया आंकड़ा मिलेगा।

## Deep Dive (गहराई में जानकारी):
रैंडम नंबर्स को पहले से ही कंप्यूटर साइंस में अहम माना जाता था। पहले हार्डवेयर बेस्ड तरीके इस्तेमाल होते थे, पर अब सॉफ्टवेयर एल्गोरिथ्म्स यह काम करते हैं। `random` मॉड्यूल एक प्स्यूडो-रैंडम नंबर जेनरेटर (PRNG) है, मतलब ये पूरी तरह से अनप्रेडिक्टेबल नहीं हैं। सिक्योरिटी-सेंसिटिव एप्लीकेशन्स के लिए, `secrets` मॉड्यूल बेहतर है क्योंकि वह क्रिप्टोग्राफिकली स्ट्रांग रैंडम नंबर्स देता है।

## See Also (और जानकारी के लिए):
- Python documentation on `random`: https://docs.python.org/3/library/random.html
- More about pseudorandomness: https://en.wikipedia.org/wiki/Pseudorandom_number_generator
- Secrets module for cryptographic use: https://docs.python.org/3/library/secrets.html