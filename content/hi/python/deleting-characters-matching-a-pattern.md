---
title:                "पैटर्न से मिलते जुलते वर्णों को हटाना"
html_title:           "Elixir: पैटर्न से मिलते जुलते वर्णों को हटाना"
simple_title:         "पैटर्न से मिलते जुलते वर्णों को हटाना"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/python/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

मैचिंग विधि से वर्ण मिटाना, यहाँ हम एक स्ट्रिंग या एक टेक्स्ट से विशेष किसी पैटर्न के अनुसार क्यूनी हटाते हैं। प्रोग्रामर्स इस प्रक्रिया का उपयोग अनावश्यक डेटा को हटाने और महत्वपूर्ण डेटा की खोज करने के लिए करते हैं।

## कैसे करें:

```Python
import re
input_string = "नमस्ते, मुझे Python पसंद है।"
pattern = '[^a-zA-Z0-9 ािीुूेैोौंँ]'
output = re.sub(pattern, '', input_string)
print(output)
```

ऊपर दिए गए कोड का आउटपुट होगा: 
```
नमस्ते मुझे Python पसंद है
```

## गहरी डाइव

1. हिस्टोरिकल कॉन्टेक्स्ट: Python भाषा में विधि मिटाने या बदलने की क्षमता वास्तव में Perl की रेगुलर एक्सप्रेसन क्षमता से प्रभावित है। 
2. विकल्प: आप `replace()` फ़ंक्शन का भी उपयोग कर सकते हैं, लेकिन यहां आपको रेप्लेस करने के लिए केवल निश्चित स्ट्रिंग मिलेगी। 
3. इंप्लीमेंटेशन विवरण: `re.sub()` फ़ंक्शन का उपयोग करके Python में यह काम किया जाता है। 

## भी देखें

रेगुलर एक्सप्रेसन से सम्बंधित अन्य स्रोतों को देखने के लिए, अगले लिंक्स का उपयोग करें:

1. [Python Documentation for re module](https://docs.python.org/3/library/re.html)
2. [Python Regular Expression HOWTO](https://docs.python.org/3/howto/regex.html)
3. [Python Regular Expressions – Real World Examples](https://www.datacamp.com/community/tutorials/python-regular-expression-tutorial)