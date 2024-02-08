---
title:                "स्ट्रिंग को कैपिटलाइज करना"
aliases:
- hi/python/capitalizing-a-string.md
date:                  2024-02-03T19:07:25.227431-07:00
model:                 gpt-4-0125-preview
simple_title:         "स्ट्रिंग को कैपिटलाइज करना"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/python/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?
एक स्ट्रिंग को बड़े अक्षरों में परिवर्तित करना मतलब है कि एक स्ट्रिंग के पहले अक्षर को ऊपरी मामले (uppercase) में और बाकी को नीचे के मामले (lowercase) में बदलना। यह क्रिया आमतौर पर इनपुट को सामान्यीकृत करने या शीर्षकों, नामों और ऐसी चीजों की पठनीयता बढ़ाने के लिए डेटा प्रसंस्करण में उपयोग की जाती है।

## कैसे:

### पायथन के अंतर्निहित विधि का उपयोग करना:
पायथन में स्ट्रिंग्स के लिए इस कार्य को आसानी से पूरा करने के लिए एक अंतर्निहित विधि `.capitalize()` है।

```python
my_string = "hello world"
capitalized_string = my_string.capitalize()
print(capitalized_string)
```
**आउटपुट:**
```
Hello world
```

### एकाधिक शब्दों को संभालना:
जहां आप चाहते हैं कि एक स्ट्रिंग में प्रत्येक शब्द एक बड़े अक्षर से शुरू हो (जैसे कि शीर्षकों में), `.title()` विधि लागू की जा सकती है।

```python
my_title = "python programming essentials"
title_case = my_title.title()
print(title_case)
```
**आउटपुट:**
```
Python Programming Essentials
```

### तृतीय-पक्ष पुस्तकालयों का उपयोग करना:
जबकि पायथन की मानक पुस्तकालय मूल स्ट्रिंग कैपिटलिज़ेशन हेतु युक्त है, `textblob` जैसे पुस्तकालय प्राकृतिक भाषा प्रसंस्करण के लिए विशेष रूप से अधिक सूक्ष्म नियंत्रण प्रदान कर सकते हैं।

पहले, सुनिश्चित करें कि आपके पास `textblob` स्थापित है:
```bash
pip install textblob
```

फिर, इसका उपयोग करके स्ट्रिंग को बड़े अक्षरों में बदलें, ध्यान रहे कि `textblob` का कैपिटलाइज़ उपयोग के संदर्भ के आधार पर विभिन्न रूप से काम कर सकता है:

```python
from textblob import TextBlob

my_sentence = "this is a test sentence"
blob = TextBlob(my_sentence)
capitalized_blob = TextBlob(blob.string.capitalize())
print(capitalized_blob)
```
**आउटपुट:**
```
This is a test sentence
```

याद रखें, जबकि `capitalize()` और `title()` विधियां सर्वव्यापी रूप से उपयोगी हैं, `textblob` जैसे पुस्तकालयों का उपयोग विशिष्ट अनुप्रयोगों के लिए अतिरिक्त लचीलापन प्रदान कर सकता है।
