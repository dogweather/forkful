---
date: 2024-01-20 17:48:29.601294-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902? (How to:) ."
lastmod: '2024-03-13T22:44:51.575007-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0915\u0940 \u0932\u0902\
  \u092C\u093E\u0908 \u091C\u094D\u091E\u093E\u0924 \u0915\u0930\u0928\u093E"
weight: 7
---

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
