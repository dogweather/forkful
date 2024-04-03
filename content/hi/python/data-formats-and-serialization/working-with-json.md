---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:54.833440-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: \u092A\u093E\u092F\
  \u0925\u0928 \u0915\u0940 \u092C\u093F\u0932\u094D\u091F-\u0907\u0928 `json` \u0932\
  \u093E\u0907\u092C\u094D\u0930\u0947\u0930\u0940 \u092A\u093E\u092F\u0925\u0928\
  \ \u0911\u092C\u094D\u091C\u0947\u0915\u094D\u091F\u094D\u0938 \u0915\u094B JSON\
  \ \u092E\u0947\u0902 \u090F\u0928\u094D\u0915\u094B\u0921\u093F\u0902\u0917 (\u092C\
  \u0926\u0932\u0928\u093E) \u0914\u0930 JSON \u0938\u0947 \u092A\u093E\u092F\u0925\
  \u0928 \u0911\u092C\u094D\u091C\u0947\u0915\u094D\u091F\u094D\u0938 \u092E\u0947\
  \u0902 \u0921\u093F\u0915\u094B\u0921\u093F\u0902\u0917 (\u092C\u0926\u0932\u0928\
  \u093E) \u0915\u0940 \u092A\u094D\u0930\u0915\u094D\u0930\u093F\u092F\u093E\u2026"
lastmod: '2024-03-13T22:44:51.627617-06:00'
model: gpt-4-0125-preview
summary: "\u092A\u093E\u092F\u0925\u0928 \u0915\u0940 \u092C\u093F\u0932\u094D\u091F\
  -\u0907\u0928 `json` \u0932\u093E\u0907\u092C\u094D\u0930\u0947\u0930\u0940 \u092A\
  \u093E\u092F\u0925\u0928 \u0911\u092C\u094D\u091C\u0947\u0915\u094D\u091F\u094D\u0938\
  \ \u0915\u094B JSON \u092E\u0947\u0902 \u090F\u0928\u094D\u0915\u094B\u0921\u093F\
  \u0902\u0917 (\u092C\u0926\u0932\u0928\u093E) \u0914\u0930 JSON \u0938\u0947 \u092A\
  \u093E\u092F\u0925\u0928 \u0911\u092C\u094D\u091C\u0947\u0915\u094D\u091F\u094D\u0938\
  \ \u092E\u0947\u0902 \u0921\u093F\u0915\u094B\u0921\u093F\u0902\u0917 (\u092C\u0926\
  \u0932\u0928\u093E) \u0915\u0940 \u092A\u094D\u0930\u0915\u094D\u0930\u093F\u092F\
  \u093E \u0915\u094B \u0938\u0930\u0932 \u092C\u0928\u093E\u0924\u0940 \u0939\u0948\
  \u0964 \u0906\u092A \u0907\u0938\u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0907\
  \u0938 \u092A\u094D\u0930\u0915\u093E\u0930 \u0915\u0930 \u0938\u0915\u0924\u0947\
  \ \u0939\u0948\u0902."
title: "JSON \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\u092E \u0915\u0930\u0928\
  \u093E"
weight: 38
---

## कैसे करें:
पायथन की बिल्ट-इन `json` लाइब्रेरी पायथन ऑब्जेक्ट्स को JSON में एन्कोडिंग (बदलना) और JSON से पायथन ऑब्जेक्ट्स में डिकोडिंग (बदलना) की प्रक्रिया को सरल बनाती है। आप इसका उपयोग इस प्रकार कर सकते हैं:

### पायथन ऑब्जेक्ट्स को JSON में एन्कोडिंग:
```python
import json

data = {
    "name": "जॉन डो",
    "age": 30,
    "isEmployee": True,
    "addresses": [
        {"city": "न्यू यॉर्क", "zipCode": "10001"},
        {"city": "सैन फ्रांसिस्को", "zipCode": "94016"}
    ]
}

json_string = json.dumps(data, indent=4)
print(json_string)
```

**आउटपुट:**

```json
{
    "name": "जॉन डो",
    "age": 30,
    "isEmployee": true,
    "addresses": [
        {
            "city": "न्यू यॉर्क",
            "zipCode": "10001"
        },
        {
            "city": "सैन फ्रांसिस्को",
            "zipCode": "94016"
        }
    ]
}
```

### JSON को पायथन ऑब्जेक्ट्स में डिकोडिंग:
```python
json_string = '''
{
    "name": "जॉन डो",
    "age": 30,
    "isEmployee": true,
    "addresses": [
        {
            "city": "न्यू यॉर्क",
            "zipCode": "10001"
        },
        {
            "city": "सैन फ्रांसिस्को",
            "zipCode": "94016"
        }
    ]
}
'''

data = json.loads(json_string)
print(data)
```

**आउटपुट:**

```python
{
    'name': 'जॉन डो', 
    'age': 30, 
    'isEmployee': True, 
    'addresses': [
        {'city': 'न्यू यॉर्क', 'zipCode': '10001'}, 
        {'city': 'सैन फ्रांसिस्को', 'zipCode': '94016'}
    ]
}
```

### तृतीय-पक्ष पुस्तकालयों के साथ काम करना:
जटिल JSON हैंडलिंग के लिए, जैसे कि स्कीमा वैलिडेशन या URL से सीधे JSON फाइलों को पार्स करना, `requests` के लिए HTTP अनुरोधों के लिए और `jsonschema` के लिए वैलिडेशन, पुस्तकालय उपयोगी हो सकते हैं।

#### `requests` के साथ एक उदाहरण JSON को URL से पार्स करने के लिए:
```python
import requests

response = requests.get('https://api.example.com/data')
data = response.json()

print(data)
```

यह स्निपेट दिए गए URL से JSON डेटा को लाता है और इसे सीधे एक पायथन ऑब्जेक्ट में परिवर्तित करता है।

#### JSON को वैलिडेट करने के लिए `jsonschema` का उपयोग करना:
पहले, वाया पिप लाइब्रेरी इंस्टॉल करें:

```bash
pip install jsonschema
```

फिर, इसे इस प्रकार उपयोग करें:

```python
from jsonschema import validate
import jsonschema

schema = {
    "type": "object",
    "properties": {
        "name": {"type": "string"},
        "age": {"type": "number"},
        "isEmployee": {"type": "boolean"},
    },
    "required": ["name", "age", "isEmployee"]
}

# मानते हैं `data` JSON डिकोडिंग से प्राप्त एक शब्दकोश है
try:
    validate(instance=data, schema=schema)
    print("मान्य JSON डेटा।")
except jsonschema.exceptions.ValidationError as err:
    print("वैलिडेशन त्रुटि:", err)
```

यह उदाहरण आपके पायथन शब्दकोश (डिकोडेड JSON डेटा से प्राप्त) को एक पूर्वनिर्धारित स्कीमा के विरुद्ध वैलिडेट करता है, यह सुनिश्चित करते हुए कि डेटा अपेक्षित फॉर्मेट्स और प्रकारों के अनुरूप है।
