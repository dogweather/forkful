---
title:                "Json के साथ काम करना"
html_title:           "Python: Json के साथ काम करना"
simple_title:         "Json के साथ काम करना"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/python/working-with-json.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
JSON काम किस लिए किया जाता है और क्यों प्रोग्रामर्स इसको करते हैं यह समझाने के लिए होता है। यह एक सरल और प्रभावी तरीका है डेटा को स्टोर और ट्रांसफर करने का, जिसमें अधिकांश प्रोग्रामिंग भाषाओं और एप्लिकेशन्स इसका समर्थन करते हैं।

## कैसे करें:
आइए हम आसान उदाहरण के साथ यह देखें कि Python में JSON काम कैसे करता है। उदाहरण के लिए, हम एक JSON फ़ाइल बनाते हैं जिसमें टेक्स्ट और संख्याओं को स्टोर किया जाता है:

```Python
import json

# JSON फ़ाइल बनाएँ
data = {
    'name': 'John',
    'age': 25,
    'hobbies': ['reading', 'coding', 'traveling']
}

with open('data.json', 'w') as file:
    json.dump(data, file)
```

और इस फ़ाइल से डेटा को रीड करते हैं:

```Python
# JSON फ़ाइल से डेटा लोड करें
with open('data.json') as file:
    data = json.load(file)

# हम अब डेटा को प्रिंट कर सकते हैं
print(data['name']) # John
print(data['age']) # 25
print(data['hobbies']) # ['reading', 'coding', 'traveling']
```

## गहराई तक प्रवेश करें:
JSON, या "जेसन", 1990s में प्रस्तुतियों को खुश करने के लिए Netscape के इंजीनियर Douglas Crockford द्वारा डेवलप किया गया। यह बाद में ECMA-404 के तहत एक स्टैंडर्ड बन गया जोर दिया गया है। कुछ अल्टरनेटिव जैसे XML भी हैं जिन्हें डेटा स्टोरिंग के लिए इस्तेमाल किया जा सकता है लेकिन वे काफी अधिक complex हैं। Python 2.6+ में डिफॉल्ट रूप से JSON प्रोवाइड किया जाता है और यह बहुत आसानी से इस्तेमाल किया जा सकता है।

## भीड़ को भी देखें:
और जानने के लिए, आप मदद फ़ाइल और विभिन्न पाठ्यक्रमों का उपयोग कर सकते हैं। यदि आप और गहराई तक जानना चाहते हैं, तो JSON.org पर संपूर्ण स्पेसिफिकेशन उपलब्ध है। आप भी इसके लिए कोड स्निपेट्स और और उपयोगी त्वरित शब्दावली खोज सकते हैं।