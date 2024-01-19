---
title:                "यादृच्छिक संख्याओं का निर्माण"
html_title:           "Clojure: यादृच्छिक संख्याओं का निर्माण"
simple_title:         "यादृच्छिक संख्याओं का निर्माण"
programming_language: "Python"
category:             "Python"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/python/generating-random-numbers.md"
---

{{< edit_this_page >}}

## क्या & क्यों?

"रैंडम नंबर गेनरेट" का मतलब ये है की आप अनुमानातीत तरीके से कुछ नंबर प्राप्त करते हैं, जिसमें कोई पैटर्न नहीं पाया जाता है। प्रोग्रामर इसे इसलिए करते हैं ताकि उन्हें डाटा के अनुमान को टेस्ट करने और सिमुलेशन करने के लिए अनियामित नमूने मिल सकें।

## कैसे करें:

Python में रैंडम नंबर उत्पन्न करने के लिए `random` मॉड्यूल का उपयोग किया जाता है। 

```Python
import random

# Random float:  0.0 <= number < 1.0
print("Random float:", random.random()) 

# Random float:  2.5 <= number < 10.0
print("Random float within a range:", random.uniform(2.5, 10.0))

# Random integer: 100 <= number <= 999
print("Random integer within a range:", random.randint(100, 999))
```

## गहरा पलुंगन

पाइथन के `random` मॉडूल का निर्माण 1991 में ग्वीडो वैन रोस्सम ने किया था, जो पाइथन की भाषा के निर्माणकर्ता हैं। इस मॉडूल के विकल्प में `numpy.random` और `random` मॉडूल में `randrange()`, `randomint()`, `choice()`, `shuffle()` आदि फंक्शन शामिल हैं। इन फंक्शन्स का चयन आवश्यकतानुसार निर्दिष्ट संख्याओं के "रैंडम" समूह की उत्पादन के लिए होता है।

## रिलेटेड सोर्स देखें:

1. Python के ऑफिसियल डॉक्यूमेंट को देखें: [https://docs.python.org/3/library/random.html](https://docs.python.org/3/library/random.html)
2. `random` मॉडूल के बारे में अधिक जानकारी के लिए: [https://www.w3schools.com/python/ref_random_randrange.asp](https://www.w3schools.com/python/ref_random_randrange.asp)
3. 'Numpy' के बारे में भी पढ़ें: [https://numpy.org/doc/stable/reference/random/index.html](https://numpy.org/doc/stable/reference/random/index.html)