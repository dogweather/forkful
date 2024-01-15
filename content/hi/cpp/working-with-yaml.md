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

## क्यों

YAML, जाकर "यामल" के रूप में जाना जाता है, एक आसान और प्रतिष्ठित तरीका है अपनी डेटा को ऑब्जेक्ट में स्टोर करने के लिए C++ प्रोग्रामर्स के लिए। यह टेक्स्ट फॉर्मेटिंग दस्तावेज़ों के लिए भी उपयोगी हो सकता है जैसे HTML, जावास्क्रिप्ट और प्रोग्रामिंग भाषाओं के अनुवाद पर काम करने के लिए।

## कैसे करें

यामल को C++ में दो तरीकों से इम्प्लीमेंट किया जा सकता है। पहली, आप थर्ड पार्टी YAML पार्सर लाइब्रेरी का उपयोग कर सकते हैं जैसे "libyaml", "yaml-cpp" या "tiny yaml"। दूसरी, आप स्वयं YAML पार्सर लिख सकते हैं जो अपने प्रोजेक्ट की आवश्यकताओं से अनुकूलित हो। नीचे दिए गए प्रोग्रामिंग उदाहरणों में, हम "yaml-cpp" लाइब्रेरी का उपयोग करेंगे। "yaml-cpp" को अपने प्रोजेक्ट में शामिल करने के लिए, आपको सिर्फ इसे अपने कोड के साथ कॉंपाइल करना होगा।

```C++
#include "yaml-cpp/yaml.h"
#include <iostream>

int main() {
  // YAML की सुरुआत करने के लिए, हम YAML डॉक्यूमेंट को लोड करते हैं
  YAML::Node doc = YAML::LoadFile("example.yaml");

  // सभी स्ट्रिंग की सूची प्रिंट करने के लिए, हम YAML कॉलेक्शन्स का उपयोग करते हैं
  std::cout << "All Strings: " << std::endl;
  for (auto str : doc["strings"]) {
    std::cout << "- " << str.as<std::string>() << std::endl;
  }

  // इंटीजर्स और बूलियन्स को प्रिंट करने के लिए, हम सीधे वस्तुओं के साथ काम करते हैं
  std::cout << "Num: " << doc["number"].as<int>() << std::endl;
  std::cout << "Bool: " << doc["bool"].as<bool>() << std::endl;