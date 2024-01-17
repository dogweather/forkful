---
title:                "Json के साथ काम करना"
html_title:           "C: Json के साथ काम करना"
simple_title:         "Json के साथ काम करना"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/working-with-json.md"
---

{{< edit_this_page >}}

## JSON क्या है और क्यों करते हैं?
JSON (JavaScript Object Notation) क्या होता है और क्यों और कैसे प्रोग्रामर्स द्वारा उपयोग किया जाता है, इसके बारे में आम लोगों को काफी समझना भी आसान है। JSON पीछले दशक में जावास्क्रिप्ट के साथ जुड़ने और वेब डेवलपमेंट में एक प्रमुख भूमिका निभाने के साथ साथ अन्य प्रोग्रामिंग भाषाओं पर भी बढ़ती मान्यता के कारण बहुत ही लोकप्रिय हो गया है।

## कैसे करें:
```C
#include <stdio.h>
#include <stdlib.h>
#include <json.h>

int main()
{
  // Sample JSON data
  char *json_data = "{ \"name\": \"John Smith\", \"age\": 30, \"city\": \"New York\" }";
  
  // Parsing the JSON data
  json_value* value = json_parse(json_data, strlen(json_data));
  
  // Printing the parsed data
  printf("Name: %s\n", json_stringify(value->u.object.values[0].value));
  printf("Age: %d\n", value->u.object.values[1].value->u.integer); 
  printf("City: %s\n", json_stringify(value->u.object.values[2].value));
  
  return 0;
}
```
```
Output:
Name: John Smith
Age: 30
City: New York
```

## गहराई में जाएं:
JSON एक लाइटवेट और आसान फॉर्मेट है जो गणनीय तरीके से डाटा को स्टोर और ट्रांसपोर्ट करने के लिए डिज़ाइन किया गया है। यह वर्तमान में वेब डेवलपमेंट और अन्य डेटा संबंधित डिवेलपमेंट्स में एक स्टैण्डर्ड बना हुआ है। इसका प्रयोग करने के लिए अन्य विकल्प भी उपलब्ध हैं जैसे XML, CSV आदि, लेकिन JSON किसी भी प्रोग्रामिंग भाषा में सरलता से इम्प्लीमेंट किया जा सकता है और यह लाइटवेट और पढ़ने में आसान होता है।

## और देखें:
- [JSON के बारे में और जानें](https://www.json.org/json-en.html)
- [C में JSON पार्सिंग कैसे करें?](https://www.codeproject.com/Articles/20027/JSON-Serialization-and-Deserialization-in-C)
- [JSON पर JSON प्रोजेक्ट](https://github.com/json-c/json-c)