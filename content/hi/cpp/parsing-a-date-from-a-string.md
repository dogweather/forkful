---
title:                "स्ट्रिंग से दिनांक पार्स करना"
date:                  2024-01-20T15:35:04.202749-07:00
simple_title:         "स्ट्रिंग से दिनांक पार्स करना"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
पार्सिंग ए डेट फ्रॉम अ स्ट्रिंग यानी एक स्ट्रिंग में से तारीख निकालना होता है। प्रोग्रामर इसे इसलिए करते हैं क्योंकि अक्सर डेटा सोर्सेज से मिलने वाली तारीखें टेक्स्ट फॉर्मेट में होती हैं और उन्हें सही फॉर्मेट में फॉर्मेट करना होता है।

## How to: (कैसे करें:)
```C++
#include <iostream>
#include <chrono>
#include <sstream>
#include <iomanip>

int main() {
    std::string dateStr = "2023-04-05";
    std::tm tm = {};
    std::stringstream ss(dateStr);

    ss >> std::get_time(&tm, "%Y-%m-%d"); // ISO 8601 format
    if(ss.fail()) {
        std::cout << "Date parsing failed." << std::endl;
        return 1;
    }

    std::chrono::system_clock::time_point tp = std::chrono::system_clock::from_time_t(std::mktime(&tm));

    // For demonstration purposes, let's print the parsed date as time since epoch
    std::cout << "Parsed date as timestamp: " << std::chrono::duration_cast<std::chrono::seconds>(tp.time_since_epoch()).count() << std::endl;

    return 0;
}

// Expected output: 
// Parsed date as timestamp: 1678051200
```

## Deep Dive (गहन जांच):
तारीख की पार्सिंग एक पुरानी समस्या है, जिसे C++ के साथ स्ट्रिंग मैनिपुलेशन और डेट-टाइम लाइब्रेरीज के जरिए हल किया जा सकता है। C++11 के बाद से, `<chrono>` लाइब्रेरी में जोड़े गए `std::chrono` नाम स्पेस में `system_clock` का उपयोग कर समय को और भी सटीक तरीके से हैंडल किया जा सकता है। पुराने तरीके जैसे `strptime` या `strftime` अभी भी काम में लाए जाते हैं लेकिन सी++ में `<iomanip>` और `std::get_time` के साथ मॉडर्न एप्रोच काफी लोकप्रिय है। इसके अल्टरनेटिव के तौर पर, बूस्ट लाइब्रेरी या थर्ड-पार्टी पार्सिंग लाइब्रेरीज जैसे कि `date.h` का इस्तेमाल भी किया जा सकता है।

## See Also (और भी देखें):
- C++ `<chrono>` library: https://en.cppreference.com/w/cpp/chrono
- C++ `<iomanip>` library: https://en.cppreference.com/w/cpp/io/manip
- C++ Date and Time tutorial: https://www.cplusplus.com/reference/ctime/
- Boost Date_Time library: https://www.boost.org/doc/libs/release/libs/date_time/
