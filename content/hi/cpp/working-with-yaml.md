---
title:                "Yaml के साथ काम करना"
html_title:           "C++: Yaml के साथ काम करना"
simple_title:         "Yaml के साथ काम करना"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/working-with-yaml.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

YAML के साथ काम करना एक बहुत ही महत्वपूर्ण भूमिका है। यह कंप्यूटर और आपके द्वारा किए गए कार्यों को संपादित और स्टोर करने का एक तरीका है। यह जानने में मदद करता है कि किस प्रकार से आपका डेटा अभिन्न संरचनाओं में संगठित है।

## कैसे करें:

```C++
#include <iostream> 
#include <yaml-cpp/yaml.h> 

int main() 
{ 
    // YAML फ़ाइल को अपने कंप्यूटर पर खोलें 
    YAML::Node node = YAML::LoadFile("example.yaml"); 

    // डेटा को वापस सेरियलाइज करें 
    std::cout << node["name"].as<std::string>() << "\n"; 
    std::cout << node["age"].as<int>() << "\n"; 

    return 0; 
} 
```
**उपलब्ध output:**

```
John
25
```

## गहराई में जायें:

YAML की शुरुआत 2001 में की गई थी जब एक प्रोग्रामर ने इसे Shell स्क्रिप्टिंग की तरह प्रयोग करने के लिए बनाया था। इसे संपादित करने के लिए आपके पास बहुत सारे विकल्प हैं, जैसे JSON या XML लेकिन YAML का जिक्र करने पर आपको स्पष्ट होगा कि इसके उपयोग से कितने ही बड़े फाइलों को आसानी से संपादित किया जा सकता है। इसका उपयोग अधिकतर कॉन्फ़िगरेशन फाइलों में किया जाता है जो साफ़ रूप से अनुशासित होते हैं।

## देखें भी:

- [YAML का आधिकारिक वेबसाइट](https://yaml.org/)
- [YAML को समर्थित भाषाएं](https://en.wikipedia.org/wiki/YAML#Supported_programming_languages)