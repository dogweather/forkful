---
title:                "यामल के साथ काम करना"
html_title:           "C#: यामल के साथ काम करना"
simple_title:         "यामल के साथ काम करना"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/python/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
YAML, 'YAML Ain't Markup Language' का छोटा रूप है, जो डेटा सीरियलाइजेशन के लिए एक मानक है। प्रोग्रामर्स इसका उपयोग कॉन्फ़िग फाइलों, डेटा स्टोरेज और कई प्रकार के एप्लिकेशन्स में करते हैं, क्योंकि यह पढ़ने में आसान और लिखने में सरल होता है।

## How to: (कैसे करें:)
YAML को पायथन में पढ़ने और लिखने के लिए, `pyyaml` लाइब्रेरी का उपयोग किया जाता है:

```Python
# pyyaml लाइब्रेरी इंस्टॉल करें
!pip install pyyaml

import yaml

# YAML स्ट्रिंग को पायथन डिक्शनरी में बदलें
yaml_data = """
name: अजय
age: 30
skills:
  - पायथन
  - जावास्क्रिप्ट
"""
python_dict = yaml.safe_load(yaml_data)
print(python_dict)

# पायथन डिक्शनरी को YAML में बदलें
dict_to_yaml = yaml.dump(python_dict, allow_unicode=True)
print(dict_to_yaml)
```

सैंपल आउटपुट:
```Python
{'name': 'अजय', 'age': 30, 'skills': ['पायथन', 'जावास्क्रिप्ट']}
name: अजय
age: 30
skills:
- पायथन
- जावास्क्रिप्ट
```

## Deep Dive (गहन जानकारी)
YAML की शुरुआत 2001 में हुई थी और धीरे-धीरे यह कॉन्फ़िगरेशन फाइलों के लिए लोकप्रिय हो गया। XML और JSON इसके विकल्प हैं, पर YAML अधिक मानव-पठनीय होने के कारण पसंद किया जाता है। YAML परिपत्र संदर्भ (circular references) को सहायता नहीं करता है और indentation से स्ट्रक्चर को define करता है।

## See Also (और देखें)
- YAML ऑफिशियल साइट: [https://yaml.org](https://yaml.org)
- PyYAML डॉक्यूमेंटेशन: [https://pyyaml.org/wiki/PyYAMLDocumentation](https://pyyaml.org/wiki/PyYAMLDocumentation)
- YAML और JSON के बीच तुलना: [https://yaml.org/spec/1.2/spec.html](https://yaml.org/spec/1.2/spec.html)
