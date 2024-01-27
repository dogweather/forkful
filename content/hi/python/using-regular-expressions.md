---
title:                "रेगुलर एक्सप्रेशन का उपयोग"
date:                  2024-01-19
html_title:           "Bash: रेगुलर एक्सप्रेशन का उपयोग"
simple_title:         "रेगुलर एक्सप्रेशन का उपयोग"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/python/using-regular-expressions.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

रेग्युलर एक्सप्रेशन (Regular expressions) पाठ डेटा में पैटर्न की खोज और मैनिपुलेशन के लिए एक तरीका है। प्रोग्रामर्स इनका उपयोग इसलिए करते हैं क्योंकि इससे जटिल पैटर्न सरलता से मिल सकते हैं और इससे डेटा वेलिडेशन, पार्सिंग या ट्रांसफॉर्मेशन कार्य कुशलता से होते हैं।

## कैसे करें:

```python
import re

# साधारण खोज उदाहरण (Simple search)
text = "आज का मौसम बहुत सुहाना है।"
pattern = 'मौसम'
match = re.search(pattern, text)

if match:
    print("पैटर्न मिला!")
else:
    print("पैटर्न नहीं मिला।")

# अंकों की खोज (Finding digits)
number_text = "संपर्क कीजिये: 1234567890"
digits_pattern = '\d+'
all_digits = re.findall(digits_pattern, number_text)
print("मिले अंक:", all_digits)
```

सैंपल आउटपुट:
```
पैटर्न मिला!
मिले अंक: ['1234567890']
```

## गहराई से जानकारी:

रेग्युलर एक्सप्रेशन की शुरूआत 1950 के दशक में हुई थी और अब यह अधिकतर प्रोग्रामिंग भाषाओं में मानक हिस्सा है। वैकल्पिक रूप से, पैटर्न मैचिंग के लिए स्ट्रिंग मेथड्स जैसे `startswith()`, `endswith()`, और `in` का उपयोग हो सकता है, परंतु ये रेग्युलर एक्सप्रेशन जितने शक्तिशाली नहीं हैं। पायथन में `re` मॉड्यूल रेगेक्स कार्यान्वयन को संभालता है, जो डायरेक्टली Perl के रेगेक्स इम्प्लीमेंटेशन से प्रेरित है।

## और भी देखें:

- [Python re Module](https://docs.python.org/3/library/re.html) - आधिकारिक पायथन डॉक्युमेंटेशन।
- [Regular Expressions in Python](https://www.regular-expressions.info/python.html) - रेग्युलर एक्सप्रेशन के बारे में गहराई से सीखने के लिए।
- [Pythex](https://pythex.org/) - पायथन रेग्युलर एक्सप्रेशन की ऑनलाइन टेस्टिंग।
