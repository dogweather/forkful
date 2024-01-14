---
title:                "Javascript: yaml के साथ काम करना"
simple_title:         "yaml के साथ काम करना"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/working-with-yaml.md"
---

{{< edit_this_page >}}

## क्यों

जब आप आज की डेवलपर्स की गलियॉं में देखें, आपको आमतौर पर डाटा और कॉन्फ़िगरेशन को याद करने के लिए YAML के बारे में कुछ तो सुना ही होगा। हालांकि यह एक सेट अप फाइल है, लेकिन यह आपको आसानी से विभिन्न प्रोग्रामिंग भाषाओं में समझाई जा सकती है। तो आइए जानते हैं कि YAML क्यों महत्वपूर्ण है और हम इसे कैसे उपयोग कर सकते हैं।

## कैसे करें

याद रखें, YAML से समझौते और सेटिंग्स खासकर उन ऐप्स और प्रोग्राम में भी किया जाता है जहाँ कॉन्फ़िगरेशन और डाटा खासकर अतिरिक्त संपादन में अधिकारी उपयोगकर्ता को होता है।

इस लेख में हम YAML के चरणों को कवर करेंगे जो निम्न हैं:

**Phpstorm** में YAML सेटिंग्स कौन से होते हैं।

`Linux/VIM/ST3` में YAML सेटिंग्स कैसे संशोधित करें।

`NodeJS` में YAML पार्‌स-अप।

`ReactJS` में किसी कॉन्फ़िगरेशन या डेटा सेटिंग के लिए YAML का प्रयोग करें।

### Example 1 - YAML संपादित करें:

```Javascript
// YAML संपादित करें
const yaml = require('js-yaml');
const fs   = require('fs');
// YAML से डेटा को अनशिफ्ट करें
const data = yaml.safeLoad(fs.readFileSync('<YAML file>', 'utf8'));
console.log(data);
// और इसे डेटा के साथ प्रभावित करें
```

मैंने एक YAML फाइल बनाई है और sample.yaml जैसा दिखता है। मुझे सभी डेटा का प्रिंट आउट चाहिए:

```JavaScript
// फाइल sample.yaml
menu:
  home:
    text: 'Home Page'
  about:
    text: 'About Page'
  contact:
    text: 'Contact Page'
services:
  - title: 'Web Design'
    description: 'Professional web design services for your business.'
  - title: 'Social Media Management'
    description: 'Increase your online presence with our social media services.'
```
परिणाम - दिया