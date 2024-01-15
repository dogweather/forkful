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

## क्यों

YAML एक लोकप्रिय मानक है जो इंटरनेट पर डेटा को संगठित और पेश करने के लिए इस्तेमाल किया जाता है। यह एक simple, human-readable फॉर्मेट है जो कि कंप्यूटर को समझने में आसान होता है और इससे डेटा को संगठित करना सरल हो जाता है। यह प्रोग्रामिंग में काफी प्रभावी है, और इससे बनाए गए डेटा संरचनाओं को सीधे अपनी कोड में इम्पोर्ट किया जा सकता है।

## कैसे करें

```TypeScript
import * as YAML from 'yaml';

// Object to YAML
const data = { name: 'John', age: 30 };
const yamlData = YAML.stringify(data);

console.log(yamlData);
// Output:
// name: John
// age: 30

// YAML to Object
const newData = YAML.parse(yamlData);

console.log(newData.name);
// Output:
// John
```

## गहराई में जाएं

YAML को सीधे ऑब्जेक्ट्स और विभिन्न डेटा संरचनाओं में कनवर्ट करने के लिए प्रोग्रामिंग में कई उपयोगी मेथड्स हैं। यह इस्तेमाल में आसान है और इससे किसी भी वेब एप्लिकेशन के लिए बेहद उपयोगी डेटा संरचनाओं को बनाया जा सकता है। यदि आप एक वेब डेवलपर हैं, तो YAML आपको अपने कोड को सरल और आसान तरीके से संगठित करने की सुविधा देता है।

## देखें भी

- YAML ऑफिशियल साइट: https://yaml.org/
- TypeScript ऑफिशियल साइट: https://www.typescriptlang.org/
- YAML का उपयोग कई अन्य प्रोग्रामिंग भाषाओं में भी होता है जैसे Python, Java, आदि। इन भाषाओं के लिए भी YAML की उपयोगिता बढ़ जाती है।