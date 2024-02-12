---
title:                "स्ट्रिंग से तारीख पार्स करना"
aliases:
- hi/cpp/parsing-a-date-from-a-string.md
date:                  2024-02-03T19:14:33.271759-07:00
model:                 gpt-4-0125-preview
simple_title:         "स्ट्रिंग से तारीख पार्स करना"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?
एक स्ट्रिंग से तारीख पर्स करना इसका मतलब है स्ट्रिंग प्रारूप की व्याख्या करना और उससे दिन, माह, और वर्ष जैसे तारीख के घटकों को निकालना। प्रोग्रामर्स यह काम उपयोगकर्ता इनपुट को संभालने, डेटा फाइलों को पढ़ने, या APIs के साथ इंटरैक्ट करने के लिए करते हैं जो तारीखों को स्ट्रिंग प्रारूप में संवाद करते हैं। यह ऐप्लिकेशन में डेटा प्रोसेसिंग, वैलिडेशन, और तारीख गणित करने के लिए अत्यंत आवश्यक है।

## कैसे करें:
आधुनिक C++ में, आप `<chrono>` लाइब्रेरी का उपयोग करके तारीखों और समयों का स्वाभाविक रूप से नियंत्रण रख सकते हैं, लेकिन यह सीधे स्ट्रिंग्स से पर्सिंग का समर्थन नहीं करती है बिना मैन्युअल पार्सिंग के ज्यादा जटिल प्रारूपों के लिए। हालांकि, ISO 8601 तारीख प्रारूपों और सरल कस्टम प्रारूपों के लिए, यहाँ है कैसे आप पार्स कर सकते हैं।

**`<chrono>` और `<sstream>` का उपयोग करते हुए:**
```cpp
#include <iostream>
#include <sstream>
#include <chrono>
#include <iomanip>

int main() {
    std::string date_str = "2023-04-15"; // ISO 8601 प्रारूप
    std::istringstream iss(date_str);
    
    std::chrono::year_month_day parsed_date;
    iss >> std::chrono::parse("%F", parsed_date);
    
    if (!iss.fail()) {
        std::cout << "पार्स की गई तारीख: " << parsed_date << std::endl;
    } else {
        std::cout << "तारीख पार्स करने में असफल।" << std::endl;
    }
    
    return 0;
}
```
नमूना आउटपुट:
```
पार्स की गई तारीख: 2023-04-15
```

ज्यादा जटिल प्रारूपों या पुराने C++ संस्करणों से निपटते समय, `date.h` (Howard Hinnant का तारीख लाइब्रेरी) जैसी तृतीय-पक्ष लाइब्रेरियां लोकप्रिय हैं। यहाँ है कैसे आप इसके साथ विभिन्न प्रारूपों में पार्स कर सकते हैं:

**`date.h` लाइब्रेरी का उपयोग करते हुए:**
लाइब्रेरी इंस्टाल कर लेना सुनिश्चित करें। आप इसे [यहाँ](https://github.com/HowardHinnant/date) पा सकते हैं।

```cpp
#include "date/date.h"
#include <iostream>

int main() {
    std::string date_str = "April 15, 2023";
    
    std::istringstream iss(date_str);
    date::sys_days parsed_date;
    iss >> date::parse("%B %d, %Y", parsed_date);
    
    if (!iss.fail()) {
        std::cout << "पार्स की गई तारीख: " << parsed_date << std::endl;
    } else {
        std::cout << "स्ट्रिंग से तारीख पार्स करने में असफल।" << std::endl;
    }

    return 0;
}
```
नमूना आउटपुट (आपके सिस्टम के लोकेल और तारीख सेटिंग्स के आधार पर भिन्न हो सकता है):
```
पार्स की गई तारीख: 2023-04-15
```
