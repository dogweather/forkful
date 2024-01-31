---
title:                "वर्तमान तारीख प्राप्त करना"
date:                  2024-01-20T15:14:09.758483-07:00
simple_title:         "वर्तमान तारीख प्राप्त करना"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
वर्तमान तारीख प्राप्त करना मतलब है कम्प्यूटर के सिस्टम क्लॉक से आज की तारीख निकालना। प्रोग्रामर्स इसे लॉग्स बनाने, यूजर के एक्शन को टाइमस्टैम्प करने या फीचर्स जैसे कैलेंडर और रिमाइंडर्स को इम्प्लिमेंट करने के लिए करते हैं।

## How to: (कैसे करें:)

```C++
#include <iostream>
#include <chrono>
#include <ctime>

int main() {
    // chrono library का इस्तेमाल करके वर्तमान समय को प्राप्त करें
    auto now = std::chrono::system_clock::now();
    // time_t में कन्वर्ट करें
    std::time_t current_time = std::chrono::system_clock::to_time_t(now);
    
    // और currentTime में प्राप्त वर्तमान समय को प्रिंट करें
    std::cout << "वर्तमान तारीख और समय: " << std::ctime(&current_time);
    return 0;
}
```

सैंपल आउटपुट:

```
वर्तमान तारीख और समय: Wed Mar 23 17:46:00 2023
```

## Deep Dive (गहराई से जानकारी)

समय प्राप्त करने के लिए C++ में `<chrono>` लाइब्रेरी मॉडर्न और सिफारिश की जाने वाली तकनीक है। पिछले दिनों में, `<ctime>` या `<time.h>` इस्तेमाल होता था, पर `<chrono>` ज़्यादा फ्लेक्सिबल और टाइप सेफ है। 

`<chrono>` C++11 में जोड़ी गई थी और यह टाइम पॉइंट्स, ड्यूरेशंस, और क्लॉक्स पर काम करती है। इसका इस्तेमाल करके, आप ऑपरेशन्स को अलग-अलग टाइम यूनिट्स में कन्वर्ट और कैलकुलेट कर सकते हैं। 

वर्तमान तारीख प्राप्त करते समय, `system_clock` हमें सिस्टम के रियल वर्ल्ड क्लॉक से समय देता है। `to_time_t` फंक्शन chrono के समय को traditional C-style time_t structure में बदल देता है, जिसे हम `ctime` के साथ पढ़ और प्रिंट कर सकते हैं।

## See Also (अन्य स्रोतों का लिंक)

समय और तारीख से संबंधित और भी क्षमताओं के लिए, नीचे दिए गए लिंक्स पर जाएं:

- C++ Reference for `<chrono>` library: [https://en.cppreference.com/w/cpp/header/chrono](https://en.cppreference.com/w/cpp/header/chrono)
- std::time documentation: [https://en.cppreference.com/w/cpp/chrono/c/time](https://en.cppreference.com/w/cpp/chrono/c/time)
- C++ Date and Time tutorial: [https://www.tutorialspoint.com/cplusplus/cpp_date_time.htm](https://www.tutorialspoint.com/cplusplus/cpp_date_time.htm)
