---
title:                "दो तारीखों की तुलना"
aliases:
- /hi/cpp/comparing-two-dates/
date:                  2024-01-20T17:33:47.496286-07:00
model:                 gpt-4-1106-preview
simple_title:         "दो तारीखों की तुलना"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
दो तारीखों की तुलना का मतलब होता है, जानना कि एक तारीख दूसरे से पहले की है या बाद की. प्रोग्रामर्स यह काम ईवेंट्स की क्रमबद्धता, डेडलाइन्स का प्रबंधन, और समय संबंधित तर्कीकरण के लिए करते हैं.

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
