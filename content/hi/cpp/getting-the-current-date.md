---
title:                "वर्तमान तारीख प्राप्त करना"
html_title:           "C#: वर्तमान तारीख प्राप्त करना"
simple_title:         "वर्तमान तारीख प्राप्त करना"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

एक प्रोग्रामर के लिए "Current Date" पाना मतलब है कंप्यूटर की सिस्टम डेट को प्राप्त करना। इसे तब किया जाता है जब आपको आपके कोड में वर्तमान तारीख की आवश्यकता हो, जैसे कि डेटाबेस लॉग/सहेजती फ़ाइलों का टाइम स्टाम्प बनाने के लिए।

## कैसे करें:

```C++
#include <iostream>
#include <chrono>
#include <ctime>

int main() {
    auto now = std::chrono::system_clock::now();
    std::time_t now_c = std::chrono::system_clock::to_time_t(now);
    std::cout << std::ctime(&now_c);
    return 0;
}
```
ऊपरी कोड उदाहरण में, हमने C++ chrono और ctime Libraries का उपयोग करके वर्तमान डेट और समय प्रिंट किया है।

## गहरा गोता:

वर्तमान डेट और समय को प्राप्त करनेका तरीका एक बहुत पुराना क्रियाकलाप है, जिसका उपयोग प्रायः लॉग फ़ाइल बनाने, यूज़र एक्टिविटी ट्रैक करने या डेटा को टाइम-स्टाम्प करने के लिए किया जाता है। इसके विकल्प में आप boost date_time library या `std::chrono` का उपयोग कर सकते हैं। `std::chrono` C++11 के बाद जोड़ा गया, जो दिनांक और समय को हैंडल करने के लिए अधिक नवीनता और क्षमता प्रदान करता है।

## अन्य स्रोत देखें:

1. [Cppreference के लिए std::chrono](https://en.cppreference.com/w/cpp/chrono)
2. [Cplusplus से ctime functions](http://www.cplusplus.com/reference/ctime/)
3. [Boost Library के लिए Date Time](https://www.boost.org/doc/libs/1_76_0/doc/html/date_time.html)