---
date: 2024-01-20 17:39:50.200154-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902) Python \u092E\
  \u0947\u0902 \u090F\u0915 string \u0915\u094B lower case \u092E\u0947\u0902 \u092C\
  \u0926\u0932\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F, `lower()` \u092E\u0947\
  \u0925\u0921 \u0915\u093E \u0907\u0938\u094D\u0924\u0947\u092E\u093E\u0932 \u0915\
  \u093F\u092F\u093E \u091C\u093E\u0924\u093E \u0939\u0948\u0964 \u0906\u0907\u090F\
  \ \u0926\u0947\u0916\u0947\u0902 \u0915\u0948\u0938\u0947."
lastmod: '2024-03-13T22:44:51.567622-06:00'
model: gpt-4-1106-preview
summary: "Python \u092E\u0947\u0902 \u090F\u0915 string \u0915\u094B lower case \u092E\
  \u0947\u0902 \u092C\u0926\u0932\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F, `lower()`\
  \ \u092E\u0947\u0925\u0921 \u0915\u093E \u0907\u0938\u094D\u0924\u0947\u092E\u093E\
  \u0932 \u0915\u093F\u092F\u093E \u091C\u093E\u0924\u093E \u0939\u0948\u0964 \u0906\
  \u0907\u090F \u0926\u0947\u0916\u0947\u0902 \u0915\u0948\u0938\u0947."
title: "\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0915\u094B \u091B\u094B\
  \u091F\u0947 \u0905\u0915\u094D\u0937\u0930\u094B\u0902 \u092E\u0947\u0902 \u092A\
  \u0930\u093F\u0935\u0930\u094D\u0924\u093F\u0924 \u0915\u0930\u0928\u093E"
weight: 4
---

## How to: (कैसे करें)
Python में एक string को lower case में बदलने के लिए, `lower()` मेथड का इस्तेमाल किया जाता है। आइए देखें कैसे:

```python
original_string = "Namaste Duniya!"
lowercase_string = original_string.lower()
print(lowercase_string)
```

इसका आउटपुट होगा:

```
namaste duniya!
```

## Deep Dive (गहराई से जानकारी)
मानवी भाषाओं में केस सेंसिटिविटी एक रोचक विशेषता है। प्रोग्रामिंग शुरू होते समय, अक्षरों को मामले (case) के अनुसार पहचानना आवश्यक था। परंतु जैसे-जैसे डाटा प्रोसेसिंग की जरूरत बढ़ी, उसके साथ ही स्ट्रिंग्स को एक समान मामले में लाने की आवश्यकता भी बढ़ी।

`.lower()` मेथड सभी बड़े अक्षरों को उनके मिलते-जुलते छोटे अक्षरों में परिवर्तित करती है। कुछ भाषाओं में, जैसे जर्मन में ऐसे भी अक्षर होते हैं, जिनके lowercase और uppercase रूप एकदम अलग होते हैं। Python का `.lower()` मेथड Unicode Standard का पालन करता है, तो यह विश्वव्यापी भाषाओं के अक्षरों को समझता और संभालता है।

Alternatives में शामिल हैं `casefold()` मेथड, जो कुछ अंतरराष्ट्रीय मामलों में `lower()` से भी अधिक शक्तिशाली हो सकता है, क्योंकि यह अधिक जटिल case मैपिंग को संभाल सकता है।

```python
# Casefold Usage
casefolded_string = original_string.casefold()
print(casefolded_string)
```

जहाँ तक implementation की बात है, Python का `.lower()` मेथड पीछे से `Py_UNICODE_TOLOWER()` फ़ंक्शन का इस्तेमाल करता है, जो इनबिल्ट है और C भाषा में लिखा गया है, जिससे यह तेज़ है।

## See Also (और जानकारी के लिए)
- Python's official documentation on string methods: [Python String Methods](https://docs.python.org/3/library/stdtypes.html#string-methods)
- Unicode Standard for understanding how Python handles different cases: [The Unicode Standard](https://www.unicode.org/standard/standard.html)
- Python Enhancement Proposals (PEP) related to string casing, like PEP 3131: [PEP 3131](https://www.python.org/dev/peps/pep-3131/)
