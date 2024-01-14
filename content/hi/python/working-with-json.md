---
title:                "Python: JSON के साथ काम करना"
simple_title:         "JSON के साथ काम करना"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/python/working-with-json.md"
---

{{< edit_this_page >}}

## क्यों

मेरा नाम है सुमित और मैं पाइथन प्रोग्रामिंग के प्रेमी हूं। आज के इस ब्लॉग पोस्ट में, हम बात करेंगे कि JSON क्या है और इससे आप कैसे अपने कोड में उपयोग कर सकते हैं। आईए जानते हैं कि यह हमारे लिए क्यों महत्वपूर्ण है।

## कैसे करें

जब हम किसी वेबसाइट पर जाते हैं तो हमें जानकारी प्राप्त करने के लिए उसके सर्वर से डेटा लेना पड़ता है। और जब हमें उस डेटा को कॉल करना होता है तो हम उसे यूआरएल से रिक्तीकरण करते हैं और उसे जेसन फॉर्मेट में पार्स करते हैं। चलिए देखते हैं कि यह चीजें कैसे करते हैं।

```Python
import json

# एक दिया गया जेसन स्ट्रिंग
json_string = '{"name": "Shiva", "age": 25, "city": "Mumbai"}'

# जेसन स्ट्रिंग को पाइथन डिक्शनरी में परिवर्तित करें
python_dict = json.loads(json_string)

# डिक्शनरी से डेटा एक्सेस करें
print("नाम:", python_dict["name"]) # नाम: शिवा
print("उम्र:", python_dict["age"]) # उम्र: 25
print("शहर:", python_dict["city"]) # शहर: मुंबई

# जेसन डिक्शनरी को स्ट्रिंग में वापस रूपांतरित करें
python_string = json.dumps(python_dict)
print(python_string) # "{'name': 'Shiva', 'age': 25, 'city': 'Mumbai'}"
```

## गहराई में जाएं

जेसन (JSON) का पूर्ण नाम जावास्क्रिप्ट ऑब्जेक्ट नोटेशन (JavaScript Object Notation) है। यह एक सरलता और अनुप्रयोगी डेटा स्टोरेज और एक्सचेंज फॉर्मेट है। आप जब भी कंप्यूटर या इंटरनेट पर JSON डेटा देखते हैं तो आप इसे डिक्शनरी के रूप में स