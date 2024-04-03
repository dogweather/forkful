---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:16:58.240463-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: C++11 \u092E\u0947\
  \u0902 \u092E\u093E\u0928\u0915 \u092A\u0941\u0938\u094D\u0924\u0915\u093E\u0932\
  \u092F, `<regex>`, \u092E\u0947\u0902 \u0928\u093F\u092F\u092E\u093F\u0924 \u0905\
  \u092D\u093F\u0935\u094D\u092F\u0915\u094D\u0924\u093F\u092F\u094B\u0902 \u0915\u0947\
  \ \u0932\u093F\u090F \u0938\u092E\u0930\u094D\u0925\u0928 \u092A\u0947\u0936 \u0915\
  \u093F\u092F\u093E \u0917\u092F\u093E, \u091C\u094B \u0938\u094D\u091F\u094D\u0930\
  \u093F\u0902\u0917 \u0916\u094B\u091C \u0914\u0930 \u0939\u0947\u0930\u092B\u0947\
  \u0930 \u0915\u0947 \u0932\u093F\u090F \u090F\u0915 \u092E\u091C\u092C\u0942\u0924\
  \ \u0922\u093E\u0902\u091A\u093E\u2026"
lastmod: '2024-03-13T22:44:52.827482-06:00'
model: gpt-4-0125-preview
summary: "C++11 \u092E\u0947\u0902 \u092E\u093E\u0928\u0915 \u092A\u0941\u0938\u094D\
  \u0924\u0915\u093E\u0932\u092F, `<regex>`, \u092E\u0947\u0902 \u0928\u093F\u092F\
  \u092E\u093F\u0924 \u0905\u092D\u093F\u0935\u094D\u092F\u0915\u094D\u0924\u093F\u092F\
  \u094B\u0902 \u0915\u0947 \u0932\u093F\u090F \u0938\u092E\u0930\u094D\u0925\u0928\
  \ \u092A\u0947\u0936 \u0915\u093F\u092F\u093E \u0917\u092F\u093E, \u091C\u094B \u0938\
  \u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0916\u094B\u091C \u0914\u0930 \u0939\
  \u0947\u0930\u092B\u0947\u0930 \u0915\u0947 \u0932\u093F\u090F \u090F\u0915 \u092E\
  \u091C\u092C\u0942\u0924 \u0922\u093E\u0902\u091A\u093E \u092A\u094D\u0930\u0926\
  \u093E\u0928 \u0915\u0930\u0924\u093E \u0939\u0948\u0964 \u092F\u0939\u093E\u0901\
  \ \u090F\u0915 \u092C\u0941\u0928\u093F\u092F\u093E\u0926\u0940 \u0909\u0926\u093E\
  \u0939\u0930\u0923 \u0939\u0948 \u091C\u094B \u090F\u0915 \u0938\u094D\u091F\u094D\
  \u0930\u093F\u0902\u0917 \u092E\u0947\u0902 \u090F\u0915 \u092A\u0948\u091F\u0930\
  \u094D\u0928 \u0915\u0940 \u0916\u094B\u091C \u0915\u0947 \u0932\u093F\u090F \u0928\
  \u093F\u092F\u092E\u093F\u0924 \u0905\u092D\u093F\u0935\u094D\u092F\u0915\u094D\u0924\
  \u093F\u092F\u094B\u0902 \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\
  \u0924\u093E \u0939\u0948."
title: "\u0930\u0947\u0917\u0941\u0932\u0930 \u090F\u0915\u094D\u0938\u092A\u094D\u0930\
  \u0947\u0936\u0928\u094D\u0938 \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\
  \u0930\u0928\u093E"
weight: 11
---

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
