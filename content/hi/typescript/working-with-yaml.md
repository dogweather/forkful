---
title:                "Yaml से काम करना"
html_title:           "TypeScript: Yaml से काम करना"
simple_title:         "Yaml से काम करना"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/working-with-yaml.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
YAML काम करने का एक तरीका है जो कि डेटा संरचना को संगत बनाता है। यह कई भाषाओं में उपयोग किया जाता है जैसे कि सी, जावा और टाइपस्क्रिप्ट। कई प्रोग्रामर यूएमएल देखने और संपादित करने के लिए इसका उपयोग करते हैं।

## कैसे करें:
```TypeScript
import YAML from 'js-yaml';
const data = YAML.load('file.yaml');
console.log(data);
```
इसके लिए, हम एक लाइब्रेरी जैसे js-yaml को इंस्टॉल करें और उससे YAML डेटा को लोड और संपादित कर सकते हैं। यहाँ हम ऊर्जा चलते हैं और कॉन्सोल पर आउटपुट देखते हैं।

## गहराई में जाएं:
कई भाषाओं में YAML का उपयोग किया जाता है, लेकिन यह पहले प्रोग्रामर्स के लिए बनाई गई नहीं गई थी। वर्तमान में, यह यूएमएल मॉडल को सबसे अधिक संबंधित और प्रतिक्रियात्मक डेटा संरचना के लिए बनाया गया है। यूएमएल कोड और यूएमएल के बीच तुलना करने का एक अभिनव तरीका है कि यूएमएल बहुत सरल होने के कारण डेटा को आसानी से पढ़ा और संपादित किया जा सकता है।

## और भी देखें:
- [js-yaml लाइब्रेरी](https://github.com/nodeca/js-yaml)
- [यूएमएल डॉक्यूमेंटेशन](https://yaml.org)
- [टाइपस्क्रिप्ट होमपेज](https://www.typescriptlang.org/)