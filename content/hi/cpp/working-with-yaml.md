---
title:                "यामल के साथ काम करना"
date:                  2024-01-19
html_title:           "C#: यामल के साथ काम करना"
simple_title:         "यामल के साथ काम करना"

category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
YAML, जिसे "YAML Ain't Markup Language" (पहले "Yet Another Markup Language") कहा जाता है, एक डाटा सीरियलाइजेशन फॉर्मैट है जो संरचित डाटा को मानव-पठनीय रूप में बयान करता है। प्रोग्रामर इसका उपयोग कॉन्फिगरेशन फाइल्स, डाटा इंटरचेंज और मेटाडाटा स्टोरेज में करते हैं, जिससे विकास प्रक्रिया सरल और मोड्यूलर हो जाती है।

## How to: (कैसे करें)
C++ में YAML के साथ काम करने के लिए `yaml-cpp` लाइब्रेरी एक लोकप्रिय विकल्प है। निचे कुछ सरल कोड के उदाहरण दिए गए हैं:

```C++
// YAML पार्सिंग के लिए सिम्पल कोड
#include <yaml-cpp/yaml.h>
#include <iostream>
#include <fstream>
#include <string>

int main() {
    std::ifstream fin("config.yaml");
    YAML::Node config = YAML::Load(fin);
    
    std::string hostname = config["hostname"].as<std::string>();
    int port = config["port"].as<int>();
    
    std::cout << "Hostname: " << hostname << "\n";
    std::cout << "Port: " << port << std::endl;

    return 0;
}
```

इससे आउटपुट ऐसा होगा जब `config.yaml` में ये सेटिंग्स हों:

```
hostname: example.com
port: 80
```

```
Hostname: example.com
Port: 80
```

## Deep Dive (गहराई से जानकारी)
YAML का जन्म 2001 में हुआ, जब XML और अन्य डाटा फॉर्मैट्स की जटिलताओं से निपटने के लिए एक सरल, मानव-पठनीय विकल्प की जरुरत महसूस की गई। JSON और TOML YAML के सबसे प्रमुख विकल्प हैं, लेकिन YAML का उपयोग उसकी रीडैबिलिटी और संरचित कॉम्प्लेक्सिटी को सहज में ढालने की क्षमता के लिए किया जाता है। `yaml-cpp` लाइब्रेरी C++ में YAML डाटा को पार्स और जनरेट करने के लिए एक कुशल टूल है, जो पर्फॉर्मेंस और सुगमता प्रदान करती है।

## See Also (और देखें)
अधिक जानकारी और सहायता के लिए निम्नलिखित स्रोत उपयोगी होंगे:

- yaml-cpp GitHub Repository: https://github.com/jbeder/yaml-cpp
- YAML Official Website: https://yaml.org
- YAML Wikipedia Page: https://en.wikipedia.org/wiki/YAML
- yaml-cpp Tutorial: https://github.com/jbeder/yaml-cpp/wiki/Tutorial
