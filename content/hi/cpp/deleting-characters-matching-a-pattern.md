---
title:                "पैटर्न से मेल खाते अक्षरों को हटाना"
aliases:
- hi/cpp/deleting-characters-matching-a-pattern.md
date:                  2024-01-20T17:42:09.425354-07:00
model:                 gpt-4-1106-preview
simple_title:         "पैटर्न से मेल खाते अक्षरों को हटाना"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
कैरेक्टर्स को डिलीट करना जो एक पैटर्न से मेल खाते हैं, मतलब एक स्ट्रिंग से कुछ खास कैरेक्टर्स को हटाना है। प्रोग्रामर्स इसे डेटा को साफ करने, जरूरी इनपुट्स को वैलिडेट और सिम्प्लिफाई करने के लिए करते हैं। 

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
