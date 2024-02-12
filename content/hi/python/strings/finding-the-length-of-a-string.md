---
title:                "स्ट्रिंग की लंबाई ज्ञात करना"
aliases:
- /hi/python/finding-the-length-of-a-string.md
date:                  2024-01-20T17:48:29.601294-07:00
model:                 gpt-4-1106-preview
simple_title:         "स्ट्रिंग की लंबाई ज्ञात करना"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/python/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों? (What & Why?)
स्ट्रिंग की लंबाई पता करना मतलब है स्ट्रिंग में कितने कैरेक्टर हैं, यह जानना। प्रोग्रामर्स इसे इसलिए करते हैं क्योंकि कई बार डेटा हेरफेर या डेटा वैलिडेशन के लिए लंबाई की जानकारी जरूरी होती है।

## कैसे करें? (How to:)
```python
# Python में स्ट्रिंग की लंबाई कैसे पाएं

my_string = "नमस्ते, पायथन!"
print(len(my_string))  # यह दिखाएगा कि स्ट्रिंग में कितने कैरैक्टर हैं

# आउटपुट: 14
```

## गहराई से जानकारी (Deep Dive)
स्ट्रिंग की लंबाई पता करने के लिए `len()` फंक्शन सबसे आम और आसान तरीका है। यह फंक्शन पायथन के प्रारंभ से ही है और अन्य डेटा संरचनाओं के लिए भी काम करता है, जैसे लिस्ट, टपल, एटसेट्रा। 

वैकल्पिक तरीकों में, आप फॉर-लूप का इस्तेमाल करके भी स्ट्रिंग की लंबाई को मैन्युअली कैलकुलेट कर सकते हैं, लेकिन यह ज्यादा कोडिंग और समय लेता है।

`len()` फंक्शन आंतरिक रूप से CPython में ऑब्जेक्ट की `__len__` मेथड का प्रयोग करता है, जो स्ट्रिंग की लंबाई को काफी तेजी से प्रदान करता है। Unicode और अन्य मल्टी-बाइट कैरैक्टस सेट के साथ, हर कैरेक्टर एक समान बाइट्स स्पेस नहीं लेता, जिसके कारण `len()` फंक्शन कैरेक्टर्स की संख्या पर आधारित लंबाई दिखाता है, न की बाइट्स की।

## सम्बंधित स्रोत (See Also)
- [Python `len()` function - Official Docs](https://docs.python.org/3/library/functions.html#len)
- [Unicode Characters in Python - Unicode HOWTO](https://docs.python.org/3/howto/unicode.html)
- [Python String Methods - W3Schools Online Web Tutorials](https://www.w3schools.com/python/python_ref_string.asp)
