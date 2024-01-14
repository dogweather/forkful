---
title:                "C++: Yaml पर काम करना"
simple_title:         "Yaml पर काम करना"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/working-with-yaml.md"
---

{{< edit_this_page >}}

# क्यों

यैमल काफी लोकप्रिय एक डाटा सीरियलाइजेशन फॉर्मैट है, जो कि बहुत से लोगों के द्वारा उपयोग किया जाता है। यह आसानी से पाठ, संपादन और साझा करने की सुविधा के साथ आता है और इसलिए हिंदी रीडर्स के लिए यह एक अच्छा प्रोग्रामिंग सत्यानाश हो सकता है।

# कैसे करें

```C++
#include <iostream>
#include <fstream>
#include "yaml-cpp/yaml.h"

int main() {
    // एक सामान्य YAML फ़ाइल खोलें
    std::ifstream file("data.yaml");

    // YAML फ़ाइल को पार्स करें
    YAML::Node data = YAML::Load(file);

    // YAML डाटा से वैल्यू ग्राहक लें
    std::string name = data["name"].as<std::string>();
    int age = data["age"].as<int>();

    // वैल्यू को उपयोग करके स्क्रीन पर दिखाएं
    std::cout << "Name: " << name << std::endl;
    std::cout << "Age: " << age << std::endl;

    return 0;
}
```

**आउटपुट:**
```
Name: राहुल शर्मा
Age: 25
```

# गहराई में जाएं

यैमल फ़ाइलें टेक्स्ट या बाइनरी फ़ॉर्मैट में लिखी जा सकती हैं और यह समर्थित भाषाओं में आसानी से बदलती हैं। यैमल की संरचना काफी सरल है और इसलिए इसे प्रोग्रामिंग भाषाओं में आसानी से पास किया जा सकता है। यैमल छोटे साइज के फ़ाइलों के लिए उपयोगी होते हैं और यह डेटा को सेव करने और संचालित करने के लिए एक अच्छा विकल्प हो सकता है।

# देखें भी

- [YAML ऑफिशियल वेबसाइट](https://yaml.org/)
- [yaml-cpp गिटहब पेज](https://github.com/jbeder/yaml-cpp)
- [यैमल का डॉक्यूमेंटेशन](https://github.com/jbeder/yaml-cpp/wiki/Tutorial)