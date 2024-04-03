---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:23:04.559461-07:00
description: "\u0915\u0948\u0938\u0947: C++ \u092E\u0947\u0902 JSON \u0915\u0947 \u0932\
  \u093F\u090F \u0915\u094B\u0908 \u092E\u0942\u0932 \u0930\u0942\u092A \u0938\u0947\
  \ \u0938\u092E\u0930\u094D\u0925\u0928 \u0928\u0939\u0940\u0902 \u0939\u0948, \u0932\
  \u0947\u0915\u093F\u0928 \u0924\u0943\u0924\u0940\u092F-\u092A\u0915\u094D\u0937\
  \ \u092A\u0941\u0938\u094D\u0924\u0915\u093E\u0932\u092F\u094B\u0902 \u091C\u0948\
  \u0938\u0947 \u0915\u0940 nlohmann/json \u0907\u0938\u0947 \u0938\u0940\u0927\u093E\
  \ \u092C\u0928\u093E\u0924\u0947 \u0939\u0948\u0902\u0964 \u0906\u0927\u093E\u0930\
  \u092D\u0942\u0924 \u0915\u093E\u0930\u094D\u092F\u094B\u0902 \u0915\u0947 \u0932\
  \u093F\u090F\u2026"
lastmod: '2024-03-13T22:44:52.882131-06:00'
model: gpt-4-0125-preview
summary: "C++ \u092E\u0947\u0902 JSON \u0915\u0947 \u0932\u093F\u090F \u0915\u094B\
  \u0908 \u092E\u0942\u0932 \u0930\u0942\u092A \u0938\u0947 \u0938\u092E\u0930\u094D\
  \u0925\u0928 \u0928\u0939\u0940\u0902 \u0939\u0948, \u0932\u0947\u0915\u093F\u0928\
  \ \u0924\u0943\u0924\u0940\u092F-\u092A\u0915\u094D\u0937 \u092A\u0941\u0938\u094D\
  \u0924\u0915\u093E\u0932\u092F\u094B\u0902 \u091C\u0948\u0938\u0947 \u0915\u0940\
  \ nlohmann/json \u0907\u0938\u0947 \u0938\u0940\u0927\u093E \u092C\u0928\u093E\u0924\
  \u0947 \u0939\u0948\u0902\u0964 \u0906\u0927\u093E\u0930\u092D\u0942\u0924 \u0915\
  \u093E\u0930\u094D\u092F\u094B\u0902 \u0915\u0947 \u0932\u093F\u090F \u0907\u0938\
  \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0948\u0938\u0947 \u0915\u0930\
  \u0947\u0902, \u092F\u0939 \u092F\u0939\u093E\u0901 \u0939\u0948."
title: "JSON \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\u092E \u0915\u0930\u0928\
  \u093E"
weight: 38
---

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
