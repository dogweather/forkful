---
title:                "एक तारीख को स्ट्रिंग में परिवर्तित करना"
html_title:           "Java: एक तारीख को स्ट्रिंग में परिवर्तित करना"
simple_title:         "एक तारीख को स्ट्रिंग में परिवर्तित करना"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

तारीख को स्ट्रिंग में बदलना, यानी डेटा प्रक्रियाकरण के दौरान दिनांक का प्रतिनिधित्व करना, एक महत्वपूर्ण कक्षा है। यह तारीखों को एक साधारण स्वरूप में परिवर्तित करना संभव करता है, जिससे वेब पेजेस, डाटाबेस और अन्य आउटपुट में उपयोग किया जा सकता है।

## कैसे करें:

```C++
#include <iostream>
#include <string>
#include <sstream>
#include <iomanip>
#include <ctime>

int main() {
    std::tm t = {};
    std::istringstream ss("2022-10-02 18:37:00");
    
    ss >> std::get_time(&t, "%Y-%m-%d %H:%M:%S");
    std::string date = std::put_time(&t, "%Y-%m-%d %H:%M:%S");
    
    std::cout << date << std::endl;

    return 0;
}
```
आउटपुट:

```C++
2022-10-02 18:37:00
```

## गहराई में:

तारीखों को स्ट्रिंग में परिवर्तित करने की अवधारणा स्वीकार्य डेटा प्रसंस्करण के इतिहास में गहरी जड़ें है। C++ ``(the current version)`` में ``std::get_time`` और ``std::put_time`` इस्तेमाल करके आप अपनी तारीख को किसी भी स्वरूप में परिवर्तित कर सकते हैं। वैकल्पिक रूप से, आप strftime का उपयोग कर सकते हैं जो POSIX C library का हिस्सा है।

## और देखें:

1. [C++ दस्तावेज़ी: स्ट्रिंग:](https://en.cppreference.com/w/cpp/string)
2. [C++ दस्तावेज़ी: टाईम:](https://en.cppreference.com/w/cpp/chrono)
3. [डेटा और समय के स्वरूपन के बारे में और जानें](https://www.cplusplus.com/reference/ctime/strftime/)