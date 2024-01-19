---
title:                "स्ट्रिंग का अंतर्कलन"
html_title:           "Arduino: स्ट्रिंग का अंतर्कलन"
simple_title:         "स्ट्रिंग का अंतर्कलन"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/python/interpolating-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
स्ट्रिंग इंटरपोलेशन, डायनेमिक वेल्यूज़ को पहले से लिखी गई स्ट्रिंग में एम्बेड करने की एक प्रक्रिया है। प्रोग्रामर्स इसे इसलिए करते हैं ताकि उन्हें अक्सर उत्पादन के लिए कोड को अद्यतित करने का आवश्यकता नहीं पड़ती।

## कैसे करें:
Python में स्ट्रिंग इंटरपोलेशन करने के लिए हम F-Strings (Formated String literals) का उपयोग कर सकते हैं।

```python
name = 'राम'
greet = f'नमस्ते, {name}'
print(greet)  
```
उत्पादन: 

```python
'नमस्ते, राम'
```

## गहरा डाइव:
#### ऐतिहासिक प्रकट:
स्ट्रिंग इंटरपोलेशन, प्राचीन प्रोग्रामिंग भाषाओं में से एक Ruby में शुरू हुआ था। Python में यह फीचर Python 3.6 के साथ जोड़ा गया।

#### बादल:
फॉर्मेट मेथड और परसेंटेज फॉर्मटिंग के रूप में Python में अन्य तरीके भी हैं, लेकिन F-Strings ने अपनी स्पष्टता और साधना द्वारा इन् सभी को पीछे छोड़ दिआ।

#### विप्लव का विवरण:
Python इंटरपोलेशन में, F-Strings लीटरल्स का उपयोग करता है और वे स्पष्ट तो हैं, लेकिन उन्हें एम्बेड करने से पहले व्यरिएबल्स और एक्स्प्रेशंस को एक्सेक्यूट करते हैं। 

## अन्य देखें:
1. [Python.org: f-Strings](https://docs.python.org/3/reference/lexical_analysis.html#f-strings)
2. [Real Python: Python String Interpolation with f-Strings](https://realpython.com/python-f-strings/)
3. [PEP 498 -- Literal String Interpolation](https://peps.python.org/pep-0498/)