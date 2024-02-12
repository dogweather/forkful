---
title:                "वर्तमान तारीख प्राप्त करना"
aliases: - /hi/cpp/getting-the-current-date.md
date:                  2024-02-03T19:10:21.392693-07:00
model:                 gpt-4-0125-preview
simple_title:         "वर्तमान तारीख प्राप्त करना"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?
C++ में वर्तमान तिथि प्राप्त करना उन प्रोग्रामों के लिए एक मौलिक कार्य है जिन्हें सिस्टम की घड़ी के आधार पर तिथियों को संसाधित या प्रदर्शित करने की आवश्यकता होती है। यह लॉगिंग, समय-संकेतन, कार्यों की अनुसूचना, और कोई भी कार्यक्षमता के लिए महत्वपूर्ण है जो तिथियों और समय पर निर्भर करती है।

## कैसे:
C++ वर्तमान तिथि प्राप्त करने के कई तरीके प्रदान करता है, जिसमें C++ स्टैंडर्ड लाइब्रेरी और थर्ड-पार्टी लाइब्रेरीज जैसे कि Boost शामिल हैं। निम्नलिखित उदाहरण इस कार्य को कैसे पूरा किया जाए यह दर्शाते हैं।

### `<chrono>` का उपयोग करते हुए (C++20 और बाद में)
C++20 ने `<chrono>` लाइब्रेरी में अधिक कार्यात्मकताएँ पेश कीं, वर्तमान तिथि प्राप्त करना सरल बना दिया:
```cpp
#include <iostream>
#include <chrono>
#include <format> // स्टड::फॉर्मैट के लिए (C++20)

int main() {
    auto current_time_point = std::chrono::system_clock::now(); // वर्तमान समय पकड़ें
    auto current_time_t = std::chrono::system_clock::to_time_t(current_time_point); // समय_t में परिवर्तित करें

    // समय को पढ़ने योग्य स्वरूप में परिवर्तित करें
    std::cout << "वर्तमान तिथि: " << std::format("{:%Y-%m-%d}", std::chrono::system_clock::to_time_t(current_time_point)) << std::endl;

    return 0;
}
```
**नमूना आउटपुट:**
```plaintext
वर्तमान तिथि: 2023-03-15
```

### `<ctime>` का उपयोग करते हुए
पुराने संस्करणों के C++ के साथ काम करने वाले प्रोग्रामरों के लिए या जो लोग पारंपरिक C लाइब्रेरी को पसंद करते हैं:
```cpp
#include <iostream>
#include <ctime>

int main() {
    std::time_t t = std::time(0); // वर्तमान समय प्राप्त करें
    std::tm* now = std::localtime(&t);
    std::cout << "वर्तमान तिथि: " 
              << (now->tm_year + 1900) << '-' 
              << (now->tm_mon + 1) << '-'
              <<  now->tm_mday
              << std::endl;

    return 0;
}
```
**नमूना आउटपुट:**
```plaintext
वर्तमान तिथि: 2023-03-15
```

### Boost Date_Time का उपयोग करते हुए
Boost लाइब्रेरीज का उपयोग करने वाली परियोजनाओं के लिए, Boost Date_Time लाइब्रेरी वर्तमान तिथि प्राप्त करने की एक वैकल्पिक विधि प्रदान करती है:
```cpp
#include <iostream>
#include <boost/date_time.hpp>

int main() {
    // Boost के Gregorian कैलेंडर का उपयोग करके वर्तमान दिन प्राप्त करें
    boost::gregorian::date today = boost::gregorian::day_clock::local_day();
    std::cout << "वर्तमान तिथि: " << today << std::endl;

    return 0;
}
```
**नमूना आउटपुट:**
```plaintext
वर्तमान तिथि: 2023-Mar-15
```
ये उदाहरण C++ में तिथियों के साथ काम करने के लिए एक बुनियादी आधार प्रदान करते हैं, जो व्यापक रेंज के अनुप्रयोगों के लिए महत्वपूर्ण है।
