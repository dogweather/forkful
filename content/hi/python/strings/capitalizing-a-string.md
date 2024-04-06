---
changelog:
- 2024-04-04, dogweather, edited
- 2024-04-04, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:02:34.962078-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: \u092A\u093E\u092F\
  \u0925\u0928 \u092E\u0947\u0902 \u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917\
  \u094D\u0938 \u0915\u094B \u0906\u0938\u093E\u0928\u0940 \u0938\u0947 \u092F\u0939\
  \ \u0915\u093E\u0930\u094D\u092F \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\u093F\
  \u090F \u090F\u0915 \u0928\u093F\u0930\u094D\u092E\u093F\u0924-\u0907\u0928 \u0935\
  \u093F\u0927\u093F `.capitalize()` \u0939\u0948\u0964."
lastmod: '2024-04-05T21:53:53.589608-06:00'
model: gpt-4-0125-preview
summary: "\u092A\u093E\u092F\u0925\u0928 \u092E\u0947\u0902 \u0938\u094D\u091F\u094D\
  \u0930\u093F\u0902\u0917\u094D\u0938 \u0915\u094B \u0906\u0938\u093E\u0928\u0940\
  \ \u0938\u0947 \u092F\u0939 \u0915\u093E\u0930\u094D\u092F \u0915\u0930\u0928\u0947\
  \ \u0915\u0947 \u0932\u093F\u090F \u090F\u0915 \u0928\u093F\u0930\u094D\u092E\u093F\
  \u0924-\u0907\u0928 \u0935\u093F\u0927\u093F `.capitalize()` \u0939\u0948\u0964."
title: "\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0915\u094B \u0915\u0948\
  \u092A\u093F\u091F\u0932\u093E\u0907\u091C \u0915\u0930\u0928\u093E"
weight: 2
---

## कैसे करें:

### पायथन के निर्मित-इन विधि का उपयोग करते हुए:
पायथन में स्ट्रिंग्स को आसानी से यह कार्य करने के लिए एक निर्मित-इन विधि `.capitalize()` है। 

```python
my_string = "hello world"
capitalized_string = my_string.capitalize()
print(capitalized_string)
```
**आउटपुट:**
```
Hello world
```

यहाँ मेरी अपनी अनुकूलित `capitalize()` है जिसे मैंने इस साइट के निर्माण के लिए उपयोग किया है। मुझे सुनिश्चित करना पड़ा कि विशेष शब्द जैसे कि **HTML** हमेशा सभी अक्षरों में रहें। इसमें [doctests](https://docs.python.org/3/library/doctest.html) का भी प्रदर्शन होता है:

```python
def capitalize(string: str) -> str:
    """
    एक स्ट्रिंग को कैपिटलाइज़ करें, अर्थात् पहले अक्षर को बड़ा करें।
    "HTML" जैसे विशेष मामलों को संभालें।

    >>> capitalize("this is html, csv, xml, and http (no REPL).")
    'This is HTML, CSV, XML, and HTTP (no REPL).'
    
    >>> capitalize("this is json, VBA, an IDE, and yaml in the CLI.")
    'This is JSON, VBA, an IDE, and YAML in the CLI.'
    """
    return (
        string
            .capitalize()
            .replace('cli',  'CLI')
            .replace('csv',  'CSV')
            .replace('html', 'HTML')
            .replace('http', 'HTTP')
            .replace('ide',  'IDE')
            .replace('json', 'JSON')
            .replace('repl', 'REPL')
            .replace('vba',  'VBA')
            .replace('xml',  'XML')
            .replace('yaml', 'YAML')
    )

```

### कई शब्दों को संभालना:
जहां आप एक स्ट्रिंग में प्रत्येक शब्द को एक बड़े अक्षर से शुरू करना चाहते हैं (जैसे कि शीर्षकों में), `.title()` विधि को लागू किया जा सकता है।

```python
my_title = "python programming essentials"
title_case = my_title.title()
print(title_case)
```
**आउटपुट:**
```
Python Programming Essentials
```

### तृतीय-पक्ष लाइब्रेरियों का उपयोग करना:
जबकि पायथन की मानक लाइब्रेरी बेसिक स्ट्रिंग कैपिटलाइज़ेशन के लिए सुसज्जित है, `textblob` जैसे लाइब्रेरियां प्राकृतिक भाषा प्रोसेसिंग के लिए अधिक सूक्ष्म नियंत्रण प्रदान कर सकती हैं।

पहले, सुनिश्चित करें कि आपके पास `textblob` स्थापित है:
```bash
pip install textblob
```

फिर, इसका उपयोग करके स्ट्रिंग्स को कैपिटलाइज़ करें, ध्यान में रखें कि `textblob` का कैपिटलाइज़ उपयोग के संदर्भ में अलग तरीके से काम कर सकता है:

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

याद रखें, जबकि `capitalize()` और `title()` विधियाँ सार्वभौमिक रूप से उपयोगी हैं, `textblob` जैसी लाइब्रेरियों का उपयोग करने से विशेष अनुप्रयोगों के लिए अतिरिक्त लचीलापन प्रदान किया जा सकता है।
