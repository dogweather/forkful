---
title:                "यामल के साथ काम करना"
html_title:           "Python: यामल के साथ काम करना"
simple_title:         "यामल के साथ काम करना"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/python/working-with-yaml.md"
---

{{< edit_this_page >}}

## क्यों

यामल काम करने के पीछे का कारण है कि इससे पाठ्यक्रम और सामग्री को संचालित किया जा सकता है, जो एक खुले स्रोत प्रोजेक्ट के साथ कठिनाई को कम करता है।

## कैसे करें

पहले से ही एक पायथन प्रोग्रामर होने के एक फायदे के रूप में, यामल कोड को समझना और उसे बदलना आसान है। इसका प्रयोग करने के लिए, आप निम्नानुसार चलाएं:

```Python
import yaml

# यामल फाइल लोड करें
with open('data.yml', 'r') as file:
    data = yaml.load(file, Loader=yaml.FullLoader)

# डेटा  प्रिंट करें
print(data)

# डेटा संशोधित करें
data['hobbies'].append('hiking')

# डेटा उपडेट करें
with open('data.yml', 'w') as file:
    yaml.dump(data, file)
```

उपरोक्त उदाहरण में, हमने पायथन में यामल लाइब्रेरी को इम्पोर्ट किया, फाइल को ओपन किया, इसे डिकोड किया और डेटा में बदलाव किया। अंत में, हमने डेटा फाइल में डम्प किया। आप अपनी उपयोगिता के अनुसार अन्य विधियों का प्रयोग कर सकते हैं।

## डीप डाइव

YAML (YAML Ain't Markup Language) एक मानक टेक्स्ट फोर्मेट है जो कि डेटा को दर्शाने के लिए उपयोग में लिया जाता है। एक YAML फाइल लेंग्थी फ़ाइल प्रोसेसिंग स्टोर और ट्रांसमिशन को कम करता हैं, और पाठ्यक्रम ऑटोमेट से फाइल कठिनाई कम करता है।

## देखें भी

- YAML काम करने के बारे में और जानने के लिए यह संरचित ट्यूटोरियल पढ़ें: [Learn X in Y minutes](https://learnxinyminutes.com/docs/yaml/)
- YAML का पूरा डोक्यूमेंटेशन साइट पर देखें: [YAML Official Documentation](https