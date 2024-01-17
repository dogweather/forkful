---
title:                "yaml से काम करना"
html_title:           "Python: yaml से काम करना"
simple_title:         "yaml से काम करना"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/python/working-with-yaml.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

यामल लेख से काम करना क्या है और क्यों प्रोग्रामर्स इसको करते हैं? यामल एक फाइल प्रारूप है जो कुछ प्रोग्रामों के साथ डेटा का उपयोग करती है, जिससे डेटा को संगठित लेकिन उपयोगकर्ता द्वारा संशोधित किया जा सकता है। यह एक आसान और स्वचालित तरीका है डेटा को प्रोग्रामिंग में उपयोग करने के लिए।

## कैसे करें:

यामल लेख को एक पाइथन स्ट्रिंग में पार्स और उसे डिक्शनरी में रूपांतरित करने के लिए निम्न संगणक को उपयोग करें:

```python
import yaml
data = yaml.safe_load("""name: John
age: 25""")
print(data)
```
आउटपुट: {'name': 'John', 'age': 25}

यामल लेख बनाएं और बंद करने के लिए निम्न पाइथन कोड भी लिखा जा सकता है:

```python
import yaml
data = {'name': 'John', 'age': 25}
yaml.safe_dump(data, open("output.yaml", "w"))
```
यह एक output.yaml फाइल बनाएगा जिसमें निम्न डेटा होगा:

```
name: John
age: 25
```

## गहराई में जाएं:

यामल १९९० के दशक में लोगों को डेटा फाइल बनाने की एक नई तकनीक के रूप में पेश किया गया था। प्रोग्रामर्स आसानी से अपने डेटा को संगठित कर सकते थे और उसे अपनी पसंद के हिसाब से संशोधित कर सकते थे। यामल के अलावा, अन्य पोपुलर डेटा फाइल प्रारूप हैं जैसे XML और JSON लेकिन यामल को प्रोग्रामिंग लिखने के लिए सबसे आसान बनाता है।

## और भी देखें:

अगर आप और अधिक जानना चाहते हैं तो निम्न स्रोतों को देख सकते हैं:

- [यामल की विकिपीडिया परिभाषा](https://hi.wikipedia.org/wiki/%E0%A4%AF%E0%A4%BE%E0%A4%AE%E0%A4%B2_(%E0%A4%B2%E0%A5%87%E0%A4%96_%E0%A4%AD%E0%A4%BE%E0%A4%B7%E0%A4%BE))
- [PyYAML डॉक्यूमेंटेशन](https://pyyaml.org/wiki/PyYAMLDocumentation)
- [यामल पाइथन लाइब्रेरी की आधिकारिक वेबसाइट](https://yaml.org/)