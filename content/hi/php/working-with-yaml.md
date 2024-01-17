---
title:                "Yaml के साथ काम करना"
html_title:           "PHP: Yaml के साथ काम करना"
simple_title:         "Yaml के साथ काम करना"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/php/working-with-yaml.md"
---

{{< edit_this_page >}}

## यह क्या है और क्यों?
YAML काम करना क्या है, इसके बारे में दो से तीन संज्ञाओं में व्याख्या करने के साथ, और यह क्यों किया जाता है कि प्रोग्रामरों यह करता है।

## कैसे करें:
`PHP…` कोड ब्लॉक के भीतर कोडिंग उदाहरण और नमूना आप्ती की तस्वीर।

```PHP
// साधारण विभाजित प्रारूप के साथ फ़ाइल से YAML डेटा लोड करें
$data = yaml_parse_file('data.yaml');
// YAML स्ट्रिंग से PHP एरे बनाएँ
$array = yaml_parse("name: John Smith \nage: 30");
```

## दीप डाइव:
ऐतिहासिक संदर्भ, वैकल्पिक और YAML काम करने के बारे में आयाम इम्पलिमेंटेशन जैसे (1) जैसे कि जानकारी का संग्रह। डेटा को सूचीबद्ध संरचना में स्विच करने के लिए बहुत सारे वैकल्पिक फॉर्मैट हैं, लेकिन YAML का उपयोग सरल सिंटैक्स और संरक्षित स्ट्रक्चर के साथ किया जाता है।

## सी ऐ भी:
सम्बंधित स्रोतों के लिए लिंक।

- [YAML.org](https://yaml.org/)
- [Wikipedia यू.ला.एम.एल.डॉट ऑडरओर्ग](https://en.wikipedia.org/wiki/YAML)