---
title:                "कंप्यूटर प्रोग्रामिंग में json के साथ काम करना"
html_title:           "Python: कंप्यूटर प्रोग्रामिंग में json के साथ काम करना"
simple_title:         "कंप्यूटर प्रोग्रामिंग में json के साथ काम करना"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/python/working-with-json.md"
---

{{< edit_this_page >}}

## क्यों

JSON एक प्रसिद्ध और लोकप्रिय डेटा फॉर्मेट है जो डेटा को स्टोर और ट्रांस्फर करने के लिए उपयोग किया जाता है। यह प्रोग्रामिंग के कई डोमेन में इस्तेमाल किया जाता है और पाठ्यक्रमों में भी शामिल होता है। इसलिए, यदि आप प्रोग्रामिंग के क्षेत्र में नए हैं या अपनी क्षमताओं को बढ़ाना चाहते हैं, तो जेएसओएन सीखना आपके लिए फायदेमंद हो सकता है।

## कैसे करें

```Python
# JSON को इम्पोर्ट करें
import json

# पाठ स्ट्रिंग को जेएसओएन ऑब्जेक्ट में कन्वर्ट करें
json_string = '{"Name": "John", "Age": 28, "Hobbies": ["Reading", "Gardening", "Cooking"]}'

# जेएसओएन ऑब्जेक्ट को पार्स करें
json_obj = json.loads(json_string)

# जेएसओएन ऑब्जेक्ट से डेटा एक्सेस करें
print(json_obj["Name"])
print(json_obj["Age"])
print(json_obj["Hobbies"])
```

आउटपुट:
```
John
28
["Reading", "Gardening", "Cooking"]
```

## गहराई में जाएँ

लोगों को जेएसओएन का उपयोग अपने Python प्रोजेक्ट में करना चाहिए, क्योंकि यह डेटा को एक सामान्य और सुलभ फॉर्मेट में स्टोर करने की सुविधा देता है। जेएसओएन ऑब्जेक्ट पाठ स्ट्रिंग का उपयोग करके डिस्कॉडर डेटा को आसानी से ट्रांस्फर कर सकते हैं। इसके अलावा, जेएसओएन बहुत सारे पॉपुलर लाइब्रेरीज़ और फ्रेमवर्क के साथ संगत है जो प्रोग्रामिंग को और भी सुविधाजनक बनाते हैं।

## और जानें

आप देख सकते हैं कि JSON को कैसे Python में उपयोग किया जाता है