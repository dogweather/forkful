---
title:                "JSON के साथ काम करना"
date:                  2024-01-19
html_title:           "Arduino: JSON के साथ काम करना"
simple_title:         "JSON के साथ काम करना"

category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/working-with-json.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

JSON (JavaScript Object Notation) एक डेटा फॉरमेट है जिससे हम डेटा को आसानी से स्टोर और एक्सचेंज कर सकते हैं। प्रोग्रामर्स इसका उपयोग APIs और वेब सर्विसेज से कम्युनिकेट करने या कॉन्फ़िगरेशन फाइल्स बनाने के लिए करते हैं।

## कैसे करें:

C++ में JSON काम करने के लिए `nlohmann/json` लाइब्रेरी बहुत प्रसिद्ध है। इसे JSON for Modern C++ भी कहते हैं। यहाँ एक उदाहरण है:

```c++
#include <iostream>
#include <nlohmann/json.hpp>

using json = nlohmann::json;

int main() {
    // JSON ऑब्जेक्ट क्रिएट करना
    json j;
    j["name"] = "Vijay";
    j["age"] = 30;
    j["is_programmer"] = true;

    // JSON ऑब्जेक्ट को स्ट्रिंग में कन्वर्ट करना
    std::string s = j.dump();   
    std::cout << "JSON string: " << s << std::endl;

    // JSON स्ट्रिंग से ऑब्जेक्ट पार्स करना
    auto parsed = json::parse(s);
    std::cout << "Parsed JSON: " " << parsed << std::endl;
    
    return 0;
}
```

सैंपल आउटपुट होगा:

```
JSON string: {"age":30,"is_programmer":true,"name":"Vijay"}
Parsed JSON: {"age":30,"is_programmer":true,"name":"Vijay"}
```

## गहन अध्ययन:

JSON का आविष्कार डगलस क्रॉकफोर्ड ने किया। यह XML जैसे भारी डेटा फॉर्मेट का एक हल्का विकल्प है। C++ के अलावा, कई दूसरी भाषाओं में इसे हैंडल करने के लिए लाइब्रेरीज उपलब्ध हैं। लेकिन C++ के चलते `nlohmann/json` की पहुंच और प्रदर्शन के कारण लोकप्रिय है।

## और भी देखें:

- JSON for Modern C++ GitHub Page: [https://github.com/nlohmann/json](https://github.com/nlohmann/json)
- JSON सीखने के लिए ऑफिशल साइट: [https://www.json.org/json-en.html](https://www.json.org/json-en.html)
