---
date: 2024-01-20 17:42:09.425354-07:00
description: "\u0915\u0948\u0930\u0947\u0915\u094D\u091F\u0930\u094D\u0938 \u0915\u094B\
  \ \u0921\u093F\u0932\u0940\u091F \u0915\u0930\u0928\u093E \u091C\u094B \u090F\u0915\
  \ \u092A\u0948\u091F\u0930\u094D\u0928 \u0938\u0947 \u092E\u0947\u0932 \u0916\u093E\
  \u0924\u0947 \u0939\u0948\u0902, \u092E\u0924\u0932\u092C \u090F\u0915 \u0938\u094D\
  \u091F\u094D\u0930\u093F\u0902\u0917 \u0938\u0947 \u0915\u0941\u091B \u0916\u093E\
  \u0938 \u0915\u0948\u0930\u0947\u0915\u094D\u091F\u0930\u094D\u0938 \u0915\u094B\
  \ \u0939\u091F\u093E\u0928\u093E \u0939\u0948\u0964 \u092A\u094D\u0930\u094B\u0917\
  \u094D\u0930\u093E\u092E\u0930\u094D\u0938 \u0907\u0938\u0947 \u0921\u0947\u091F\
  \u093E \u0915\u094B \u0938\u093E\u092B \u0915\u0930\u0928\u0947, \u091C\u0930\u0942\
  \u0930\u0940\u2026"
lastmod: '2024-03-13T22:44:52.817746-06:00'
model: gpt-4-1106-preview
summary: "\u0915\u0948\u0930\u0947\u0915\u094D\u091F\u0930\u094D\u0938 \u0915\u094B\
  \ \u0921\u093F\u0932\u0940\u091F \u0915\u0930\u0928\u093E \u091C\u094B \u090F\u0915\
  \ \u092A\u0948\u091F\u0930\u094D\u0928 \u0938\u0947 \u092E\u0947\u0932 \u0916\u093E\
  \u0924\u0947 \u0939\u0948\u0902, \u092E\u0924\u0932\u092C \u090F\u0915 \u0938\u094D\
  \u091F\u094D\u0930\u093F\u0902\u0917 \u0938\u0947 \u0915\u0941\u091B \u0916\u093E\
  \u0938 \u0915\u0948\u0930\u0947\u0915\u094D\u091F\u0930\u094D\u0938 \u0915\u094B\
  \ \u0939\u091F\u093E\u0928\u093E \u0939\u0948\u0964 \u092A\u094D\u0930\u094B\u0917\
  \u094D\u0930\u093E\u092E\u0930\u094D\u0938 \u0907\u0938\u0947 \u0921\u0947\u091F\
  \u093E \u0915\u094B \u0938\u093E\u092B \u0915\u0930\u0928\u0947, \u091C\u0930\u0942\
  \u0930\u0940 \u0907\u0928\u092A\u0941\u091F\u094D\u0938 \u0915\u094B \u0935\u0948\
  \u0932\u093F\u0921\u0947\u091F \u0914\u0930 \u0938\u093F\u092E\u094D\u092A\u094D\
  \u0932\u093F\u092B\u093E\u0908 \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\u093F\
  \u090F \u0915\u0930\u0924\u0947 \u0939\u0948\u0902\u0964."
title: "\u092A\u0948\u091F\u0930\u094D\u0928 \u0938\u0947 \u092E\u0947\u0932 \u0916\
  \u093E\u0924\u0947 \u0905\u0915\u094D\u0937\u0930\u094B\u0902 \u0915\u094B \u0939\
  \u091F\u093E\u0928\u093E"
weight: 5
---

## How to: (कैसे करें:)
```C++
#include <iostream>
#include <algorithm>
#include <string>

int main() {
    std::string data = "H3llo W@rld!";
    const std::string pattern = "1234567890@!";

    data.erase(std::remove_if(data.begin(), data.end(),
                              [&](char ch) { return pattern.find(ch) != std::string::npos; }),
               data.end());

    std::cout << data << std::endl; // Output: Hello Wrld
    return 0;
}
```

## Deep Dive (गहराई से समझिए):
स्ट्रिंग्स से कैरेक्टर्स हटाने की शुरुआत तब हुई जब पहली बार स्ट्रिंग्स का इस्तेमाल हुआ। C++ स्टैण्डर्ड लाइब्रेरी में `remove_if` और `erase` जैसे फंक्शंस यह काम आसान बनाते हैं। इसी तरह के काम के लिए अन्य भाषाओं में भी बिल्ट-इन फंक्शंस मौजूद होते हैं। C++ में लैम्ब्डा फंक्शंस का इस्तेमाल करके हम पैटर्न-मैच करके कैरेक्टर्स को जल्दी से हटा सकते हैं, जैसा ऊपर कोड में दिखाया गया है।

## See Also (और देखें):
- C++ Standard Template Library: [http://www.cplusplus.com/reference/](http://www.cplusplus.com/reference/)
- C++ Lambda Expressions: [https://en.cppreference.com/w/cpp/language/lambda](https://en.cppreference.com/w/cpp/language/lambda)
- Regular Expressions in C++ (for complex patterns): [https://en.cppreference.com/w/cpp/regex](https://en.cppreference.com/w/cpp/regex)
