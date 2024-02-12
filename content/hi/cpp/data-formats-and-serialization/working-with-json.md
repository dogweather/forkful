---
title:                "JSON के साथ काम करना"
aliases:
- /hi/cpp/working-with-json/
date:                  2024-02-03T19:23:04.559461-07:00
model:                 gpt-4-0125-preview
simple_title:         "JSON के साथ काम करना"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?

JSON (जावास्क्रिप्ट ऑब्जेक्ट नोटेशन) डेटा संग्रहीत करने और परिवहन के लिए एक हल्का प्रारूप है, जो सर्वरों और वेब अनुप्रयोगों के बीच डेटा आदान-प्रदान के लिए एक उत्कृष्ट माध्यम बनाता है। प्रोग्रामर JSON का उपयोग इसकी मानवों द्वारा आसानी से पठनीयता और मशीनों द्वारा सरलता से पार्स करने की क्षमता के कारण करते हैं, विशेषकर जब इंटरनेट पर डेटा आदान-प्रदान या कॉन्फ़िगरेशन सेटिंग्स की आवश्यकता वाले अनुप्रयोगों पर काम करते हैं।

## कैसे:

C++ में JSON के लिए कोई मूल रूप से समर्थन नहीं है, लेकिन तृतीय-पक्ष पुस्तकालयों जैसे की nlohmann/json इसे सीधा बनाते हैं। आधारभूत कार्यों के लिए इसका उपयोग कैसे करें, यह यहाँ है:

सबसे पहले, सुनिश्चित करें कि आपके पास पुस्तकालय स्थापित है। अगर आप vcpkg या Conan जैसे पैकेज मैनेजर का इस्तेमाल कर रहे हैं, तो आप आसानी से अपनी प्रोजेक्ट में `nlohmann/json` को जोड़ सकते हैं।

### स्ट्रिंग से JSON पार्स करना

```cpp
#include <iostream>
#include <nlohmann/json.hpp>

int main() {
    // स्ट्रिंग के रूप में JSON डेटा
    std::string jsonData = "{\"name\":\"John\", \"age\":30, \"city\":\"New York\"}";

    // JSON स्ट्रिंग पार्स करें
    auto jsonObject = nlohmann::json::parse(jsonData);

    // डेटा एक्सेस करना
    std::cout << "Name: " << jsonObject["name"] << "\n"
              << "Age: " << jsonObject["age"] << "\n"
              << "City: " << jsonObject["city"] << std::endl;

    return 0;
}
```

**नमूना आउटपुट:**

```
Name: John
Age: 30
City: New York
```

### JSON जेनरेट करना

JSON डेटा बनाना भी उतना ही सीधा है; आप बस `nlohmann::json` ऑब्जेक्ट को मूल्यों का आवंटन करते हैं।

```cpp
#include <nlohmann/json.hpp>
#include <iostream>

int main() {
    // एक JSON ऑब्जेक्ट बनाना
    nlohmann::json jsonObject;
    jsonObject["name"] = "Jane";
    jsonObject["age"] = 25;
    jsonObject["city"] = "Los Angeles";

    // JSON ऑब्जेक्ट को स्ट्रिंग में परिवर्तित करना और प्रिंट करना
    std::string jsonString = jsonObject.dump(4); // आकर्षक-प्रिंटिंग के लिए तर्क 4
    std::cout << jsonString << std::endl;

    return 0;
}
```

**नमूना आउटपुट:**

```
{
    "name": "Jane",
    "age": 25,
    "city": "Los Angeles"
}
```

ये उदाहरण `nlohmann/json` पुस्तकालय का उपयोग करके C++ में JSON के साथ काम करने के मूल कार्यक्षमता को प्रदर्शित करते हैं। इन आधारभूत बातों के साथ, आप विन्यास फ़ाइलों से लेकर नेटवर्क किए गए अनुप्रयोगों में डेटा आदान-प्रदान के लिए विभिन्न अनुप्रयोगों के लिए JSON पार्स कर सकते हैं और जेनरेट कर सकते हैं।
