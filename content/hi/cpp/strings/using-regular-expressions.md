---
title:                "रेगुलर एक्सप्रेशन्स का उपयोग करना"
aliases:
- /hi/cpp/using-regular-expressions.md
date:                  2024-02-03T19:16:58.240463-07:00
model:                 gpt-4-0125-preview
simple_title:         "रेगुलर एक्सप्रेशन्स का उपयोग करना"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/using-regular-expressions.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?
C++ में नियमित अभिव्यक्तियाँ वे वर्णों के अनुक्रम होते हैं जो एक खोज पैटर्न को परिभाषित करते हैं, जिनका उपयोग स्ट्रिंग मिलान या हेरफेर के लिए किया जाता है। प्रोग्रामर इनका उपयोग इनपुट की वैधता सत्यापित करने, स्ट्रिंग्स के भीतर घटनाओं की खोज, या स्ट्रिंग्स को टोकन में विभाजित करने जैसे कार्यों के लिए करते हैं, जिससे ये पाठ संसाधन के लिए एक अनिवार्य उपकरण बनते हैं।

## कैसे करें:
C++11 में मानक पुस्तकालय, `<regex>`, में नियमित अभिव्यक्तियों के लिए समर्थन पेश किया गया, जो स्ट्रिंग खोज और हेरफेर के लिए एक मजबूत ढांचा प्रदान करता है। यहाँ एक बुनियादी उदाहरण है जो एक स्ट्रिंग में एक पैटर्न की खोज के लिए नियमित अभिव्यक्तियों का उपयोग करता है:

```cpp
#include <iostream>
#include <regex>

int main() {
    std::string target = "Hello, my email is example@example.com";
    std::regex email_pattern(R"(\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,}\b)");

    if (std::regex_search(target, email_pattern)) {
        std::cout << "Email found!" << std::endl;
    } else {
        std::cout << "No email found." << std::endl;
    }

    return 0;
}
```
**नमूना आउटपुट**
```
Email found!
```

अधिक जटिल हेरफेर के लिए, जैसे कि स्ट्रिंग्स के भीतर पैटर्न को बदलना, C++ की नियमित अभिव्यक्तियाँ बहुत सहायक हो सकती हैं:

```cpp
#include <iostream>
#include <regex>

int main() {
    std::string text = "The rain in Spain falls mainly in the plain.";
    std::regex vowel_regex("([aeiou])");

    std::string replaced_text = std::regex_replace(text, vowel_regex, "*");
    std::cout << replaced_text << std::endl;

    return 0;
}
```
**नमूना आउटपुट**
```
Th* r**n *n Sp**n f*lls m**nly *n th* pl**n.
```

मानक पुस्तकालय से आगे तलाशने वाले प्रोग्रामरों के लिए, बूस्ट रेगेक्स पुस्तकालय (`boost/regex.hpp`) एक लोकप्रिय तृतीय-पक्ष विकल्प है जो जटिल पैटर्न या व्यापक डेटा प्रसंस्करण के लिए उन्नत रेगेक्स क्षमताओं और प्रदर्शन अनुकूलन प्रदान करता है:

```cpp
#include <iostream>
#include <boost/regex.hpp>

int main() {
    std::string s = "Boost libraries are fun!";
    boost::regex expr("(\\w+)\\s(libraries)"); // "Boost libraries" से मिलान
    std::string fmt("GNU \\1"); // "GNU Boost" से बदलें

    std::string result = boost::regex_replace(s, expr, fmt);
    std::cout << result << std::endl;

    return 0;
}
```
**नमूना आउटपुट**
```
GNU Boost are fun!
```

ये उदाहरण C++ की नियमित अभिव्यक्तियों के साथ क्षमताओं की सतह को खुरचते हैं, जो मानक पुस्तकालय का उपयोग करके या बूस्ट की शक्तिशाली रेगेक्स कार्यान्वयन द्वारा प्रबलित, बुनियादी खोजों, पैटर्न मिलान, और प्रतिस्थापनों का प्रदर्शन करते हैं।
