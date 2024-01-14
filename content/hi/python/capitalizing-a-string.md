---
title:                "Python: स्ट्रिंग को कैपिटलाइज़ करना"
simple_title:         "स्ट्रिंग को कैपिटलाइज़ करना"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/python/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# वजह
किसी भी प्रोग्रामिंग भाषा में, डेटा को संरचित और प्रस्तुत करने के लिए विभिन्न स्ट्रिंग प्रकार प्रयोग किए जाते हैं। एक स्ट्रिंग एक सार्वत्रिक डेटा प्रकार है जो टेक्स्ट और सिंबोल्स से बने होते हैं। जब हम किसी स्ट्रिंग को कैपिटलाइज़ करते हैं, तो हम उसके सभी अक्षरों को अपरकेस में लिखते हैं। इस प्रकार, स्ट्रिंग को सबसे पहले अक्षर से ही शानदार और प्रभावशाली दिखाया जाता है। 

## कैसे करें
```Python
string = "python programming"
print(string.capitalize())
```
**आउटपुट:**
Python programming

ऊपर दिए गए कोड से आप देख सकते हैं कि `capitalize()` फ़ंक्शन को कैसे इस्तेमाल किया जाता है। स्ट्रिंग वेरिएबल `string` को पास किया गया है और `print()` फ़ंक्शन को स्ट्रिंग के साथ जोड़कर उसका आउटपुट दिखाया गया है। 

## गहराई में
Sट्रिंग कैपिटलाइज़ फ़ंक्शन अन्य बहुत सारे स्ट्रिंग फ़ंक्शन का एक हिस्सा है जो पाइथन में उपलब्ध हैं। इसके अलावा, आप सभी कोड लाइन को संशोधित कर सकते हैं ताकि आपके द्वारा लिखे प्रोग्राम और लिब्रेरी में स्थित अन्य स्ट्रिंग फ़ंक्शनों को भी समझने में आसानी हो। 

# देखें भी
- [बुक "उद्धरण"]: https://docs.python.org/3/reference/simple_stmts.html#expression-statements
- [मॉड्यूल "स्ट्रिंग"]: https://docs.python.org/3/library/string.html
- [पाइथन "स्ट्रिंग" फ़ंक्शन]: https://www.w3schools.com/python/python_strings.asp