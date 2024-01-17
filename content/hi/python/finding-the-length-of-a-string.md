---
title:                "स्ट्रिंग की लंबाई का पता लगाना"
html_title:           "Python: स्ट्रिंग की लंबाई का पता लगाना"
simple_title:         "स्ट्रिंग की लंबाई का पता लगाना"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/python/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

#Python: स्ट्रिंग की लंबाई कैसे पता करे
## What & Why?
जब हम किसी प्रोग्राम को लिखते हैं, तो स्ट्रिंग (string) को एक अहम भाग बनाना पड़ता है। स्ट्रिंग एक अभिव्यक्ति को बताता है जैसे कि एक वाक्य या कोई शब्द। प्रोग्रामर्स स्ट्रिंग की लंबाई को पता करते हैं ताकि वो उसे अपनी आवश्यकताओं के अनुसार बदल सकें।

## How to:
अगर हमें किसी स्ट्रिंग की लंबाई पता करनी हो, तो हम इस तरह से कोड लिख सकते हैं:
```python
string = "Hello world!"
print(len(string))
```
आउटपुट:
```
12
```
जैसे कि आप देख सकते हैं, हमने ```len()``` फंक्शन का उपयोग किया है जो कि दिए गए स्ट्रिंग की लंबाई को पूरे संख्या के रूप में वापस देता है। इस तरह से हम बहुत ही आसानी से स्ट्रिंग की लंबाई पता कर सकते हैं।

## Deep Dive:
इसके अलावा, हम ये भी कर सकते हैं कि हम स्ट्रिंग की कुछ खास हिस्से की लंबाई भी पता कर सकें। जैसे कि:
```python
string = "Hello world!"
print(len(string[6:]))
```
आउटपुट:
```
6
```
इस प्रकार हमने स्ट्रिंग के 6वें अक्षर से शुरु होने वाले हिस्से की लंबाई पता की है।

## See Also:
यदि आपको पाइथन की और भी तरह की जानकारी चाहिए तो आप निम्नलिखित स्रोतों को देख सकते हैं:
- [वैकल्पिक तरीके स्ट्रिंग की लंबाई पता करने के लिए (Alternatives to finding string length)](https://www.programiz.com/python-programming/methods/string)
- [स्ट्रिंग फंक्शन्स के बारे में और जानकारी के लिए (More on string functions)](https://www.geeksforgeeks.org/python-strings/)
- [पाइथन की आधिकारिक दस्तावेज़ (Python Official Documentation)](https://docs.python.org/3/library/string.html)