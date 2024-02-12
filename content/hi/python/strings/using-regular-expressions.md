---
title:                "रेगुलर एक्सप्रेशन्स का उपयोग करना"
aliases:
- /hi/python/using-regular-expressions.md
date:                  2024-02-03T19:19:06.073331-07:00
model:                 gpt-4-0125-preview
simple_title:         "रेगुलर एक्सप्रेशन्स का उपयोग करना"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/python/using-regular-expressions.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?
रेगुलर एक्सप्रेशन (regex) पैटर्न होते हैं जिनका उपयोग स्ट्रिंग्स में अक्षर संयोजनों का मिलान करने के लिए किया जाता है। प्रोग्रामर इसका उपयोग परिभाषित पैटर्न के आधार पर टेक्स्ट को खोजने, संपादित करने, या मैनिप्युलेट करने के लिए करते हैं, जिससे डेटा वैलिडेशन, पार्सिंग, या ट्रांसफॉर्मेशन जैसे कार्यों के लिए ये अपरिहार्य हो जाते हैं।

## कैसे करें:
पायथन में regex का उपयोग `re` मॉड्यूल के जरिए किया जाता है, जो रेगुलर एक्सप्रेशन का उपयोग करके टेक्स्ट को प्रोसेस करने के लिए एक सेट ऑफ फंक्शन्स प्रदान करता है।

### मूल पैटर्न मिलान
एक स्ट्रिंग में एक पैटर्न की खोज के लिए, `re.search()` का उपयोग करें। जब पैटर्न मिलता है, तो यह एक मैच ऑब्जेक्ट लौटाता है, अन्यथा `None`.
```python
import re

text = "Learn Python programming"
match = re.search("Python", text)
if match:
    print("पैटर्न मिला!")
else:
    print("पैटर्न नहीं मिला.")
```
आउटपुट:
```
पैटर्न मिला!
```

### रेगुलर एक्सप्रेशन का संकलन
एक ही पैटर्न के बार-बार उपयोग के लिए, पहले `re.compile()` के साथ उसे संकलित करें बेहतर प्रदर्शन के लिए।
```python
pattern = re.compile("Python")
match = pattern.search("Learn Python programming")
if match:
    print("संकलित पैटर्न मिला!")
```
आउटपुट:
```
संकलित पैटर्न मिला!
```

### स्ट्रिंग्स को विभाजित करना
एक regex पैटर्न के प्रत्येक मैच पर एक स्ट्रिंग को विभाजित करने के लिए, `re.split()` का उपयोग करें।
```python
result = re.split("\s", "Python is fun")
print(result)
```
आउटपुट:
```
['Python', 'is', 'fun']
```

### सभी मैचों का पता लगाना
एक पैटर्न के सभी नॉन-ओवरलैपिंग अवसरों को खोजने के लिए, `re.findall()` का उपयोग करें।
```python
matches = re.findall("n", "Python programming")
print(matches)
```
आउटपुट:
```
['n', 'n']
```

### टेक्स्ट को रिप्लेस करना
एक पैटर्न के अवसरों को एक नए स्ट्रिंग के साथ रिप्लेस करने के लिए `re.sub()` का उपयोग करें।
```python
replaced_text = re.sub("fun", "awesome", "Python is fun")
print(replaced_text)
```
आउटपुट:
```
Python is awesome
```

### तृतीय-पक्ष लाइब्रेरीज
पायथन की निर्मित `re` मॉड्यूल शक्तिशाली होते हुए भी, तृतीय-पक्ष लाइब्रेरीज जैसे कि `regex` और अधिक सुविधाएँ और उन्नत प्रदर्शन प्रदान करते हैं। `regex` का उपयोग करने के लिए, इसे पिप के माध्यम से इंस्टॉल करें (`pip install regex`) और इसे अपने कोड में इम्पोर्ट करें।

```python
import regex

text = "Learning Python 3.8"
match = regex.search(r"Python\s(\d+\.\d+)", text)
if match:
    print(f"वर्जन मिला: {match.group(1)}")
```
आउटपुट:
```
वर्जन मिला: 3.8
```
