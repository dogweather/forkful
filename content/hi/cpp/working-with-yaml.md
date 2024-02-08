---
title:                "YAML के साथ काम करना"
date:                  2024-02-03T19:25:22.097446-07:00
model:                 gpt-4-0125-preview
simple_title:         "YAML के साथ काम करना"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?

YAML, जिसका पूरा नाम YAML Ain't Markup Language है, एक मानव-पठनीय डेटा सीरियलाइजेशन प्रारूप है। कार्यक्रमकर्ता इसका उपयोग कॉन्फिगरेशन फाइलों, डेटा डंपिंग, और पढ़ने में आसान और समझने में सरल सिंटैक्स के कारण XML या JSON की तुलना में संरचनागत डेटा स्टोर करने के लिए इसका उपयोग करते हैं।

## कैसे करें:

C++ में YAML के साथ काम करने के लिए लोकप्रिय पसंद `yaml-cpp` लाइब्रेरी है। सबसे पहले, सुनिश्चित करें कि आपके पास `yaml-cpp` स्थापित है और आपके C++ प्रोजेक्ट के साथ सही ढंग से लिंक किया गया है।

**एक YAML फाइल पढ़ना:**

```cpp
#include <iostream>
#include <fstream>
#include <yaml-cpp/yaml.h>

int main() {
    YAML::Node config = YAML::LoadFile("config.yaml");
    
    if(config["title"]) {
        std::cout << "शीर्षक: " << config["title"].as<std::string>() << std::endl;
    }
    
    return 0;
}
```

एक `config.yaml` जो इस तरह दिखती है:

```yaml
title: "उदाहरण YAML"
```

उपरोक्त C++ कोड चलाने पे प्रोड्यूस होगा:

```
शीर्षक: उदाहरण YAML
```

**एक YAML फाइल में लिखना:**

```cpp
#include <fstream>
#include <yaml-cpp/yaml.h>

int main() {
    YAML::Emitter out;
    out << YAML::BeginMap;
    out << YAML::Key << "शीर्षक" << YAML::Value << "उदाहरण YAML";
    out << YAML::EndMap;
    
    std::ofstream fout("output.yaml");
    fout << out.c_str();
    
    return 0;
}
```

यह कोड एक `output.yaml` बनाएगा जिसमें सामग्री होगी:

```yaml
title: उदाहरण YAML
```

ये उदाहरण C++ का उपयोग करके YAML फाइलों से पढ़ने और लिखने के बुनियादी परिचय के तौर पर काम करते हैं `yaml-cpp` लाइब्रेरी का उपयोग करके। अधिक जटिल संरचनाओं और उपयोग के मामलों के लिए, कृपया `yaml-cpp` डॉक्यूमेंटेशन को देखें जिसमें सीक्वेंसेस, टैग्स, और अधिक उन्नत सीरियलाइजेशन और डीसीरियलाइजेशन तकनीकों जैसी विशेषताओं का पता चलता है।
