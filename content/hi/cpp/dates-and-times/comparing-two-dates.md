---
date: 2024-01-20 17:33:47.496286-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902?) C++ \u092E\
  \u0947\u0902 `<chrono>` \u0932\u093E\u0907\u092C\u094D\u0930\u0947\u0930\u0940 \u0915\
  \u093E \u0907\u0938\u094D\u0924\u0947\u092E\u093E\u0932 \u0915\u0930\u0915\u0947\
  \ \u0924\u093E\u0930\u0940\u0916\u094B\u0902 \u0915\u0940 \u0924\u0941\u0932\u0928\
  \u093E \u0906\u0938\u093E\u0928 \u0939\u0948. \u092F\u0939\u093E\u0902 \u092A\u0930\
  \ \u090F\u0915 \u0909\u0926\u093E\u0939\u0930\u0923 \u0939\u0948."
lastmod: '2024-04-05T21:53:54.829013-06:00'
model: gpt-4-1106-preview
summary: "(\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902?) C++ \u092E\u0947\u0902\
  \ `<chrono>` \u0932\u093E\u0907\u092C\u094D\u0930\u0947\u0930\u0940 \u0915\u093E\
  \ \u0907\u0938\u094D\u0924\u0947\u092E\u093E\u0932 \u0915\u0930\u0915\u0947 \u0924\
  \u093E\u0930\u0940\u0916\u094B\u0902 \u0915\u0940 \u0924\u0941\u0932\u0928\u093E\
  \ \u0906\u0938\u093E\u0928 \u0939\u0948."
title: "\u0926\u094B \u0924\u093E\u0930\u0940\u0916\u094B\u0902 \u0915\u0940 \u0924\
  \u0941\u0932\u0928\u093E"
weight: 27
---

## How to: (कैसे करें?)
C++ में `<chrono>` लाइब्रेरी का इस्तेमाल करके तारीखों की तुलना आसान है. यहां पर एक उदाहरण है:

```C++
#include <iostream>
#include <chrono>
#include <ctime>

int main() {
    // आज की तारीख प्राप्त करें
    std::chrono::system_clock::time_point today = std::chrono::system_clock::now();
    
    // कोई तय तारीख सेट करें
    std::tm timeStruct = {};
    timeStruct.tm_year = 2023 - 1900; // वर्ष 2023
    timeStruct.tm_mon = 3; // अप्रैल (0-11, जनवरी से गिनती स्टार्ट होती है)
    timeStruct.tm_mday = 18; // 18 तारीख
    std::time_t set_time = std::mktime(&timeStruct);
    std::chrono::system_clock::time_point set_date = std::chrono::system_clock::from_time_t(set_time);
    
    // तारीखों की तुलना करें
    if (today < set_date) {
        std::cout << "आज की तारीख तय तारीख से पहले है।" << std::endl;
    } else {
        std::cout << "तय तारीख आज की तारीख से पहले या बराबर है।" << std::endl;
    }

    return 0;
}
```
आउटपुट होगा:
```
आज की तारीख तय तारीख से पहले है।
```
या
```
तय तारीख आज की तारीख से पहले या बराबर है।
```

## Deep Dive (गहराई से जानकारी)
तारीखों की तुलना C++ में `<chrono>` लाइब्रेरी के आने से पहले काफ़ी मुश्किल हुआ करती थी. पुराने C++ में `<ctime>` का इस्तेमाल होता था, जो बहुत ही कमजोर और भ्रामक था. `<chrono>` आने से तारीख और समय से जुड़े काम सरल और सुरक्षित हो गए हैं. 

इनके अलावा, दूसरी भाषाओं जैसे कि Python में `datetime` मॉड्यूल का भी ऑप्शन है जो इसी तरह के काम को आसान बनाता है. लेकिन, C++ की स्टैण्डर्ड लाइब्रेरी में `<chrono>` का प्रयोग करना ज्यादा एफीशिएंट और टाइप-सेफ है.

डेट कंपेरिज़न के पीछे का लॉजिक यह है कि यह टाइमप्वाइंट्स (time points) को एक फिक्स रेफरेंस डेट से गिना जाता है, जो की आमतौर पर यूनिक्स टाइम एपोक (1 जनवरी, 1970) होता है.

## See Also (और जानकारी के लिए)
- C++ `<chrono>` Library: [cppreference.com/w/cpp/chrono](https://en.cppreference.com/w/cpp/chrono)
- Python `datetime` Module: [python.org](https://docs.python.org/3/library/datetime.html)
