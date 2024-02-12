---
title:                "तारीख को स्ट्रिंग में बदलना"
aliases: - /hi/cpp/converting-a-date-into-a-string.md
date:                  2024-01-20T17:36:43.142494-07:00
model:                 gpt-4-1106-preview
simple_title:         "तारीख को स्ट्रिंग में बदलना"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
डेट को स्ट्रिंग में कन्वर्ट करना मतलब है कैलेंडर डेट को टेक्स्ट फॉर्मेट में बदलना। प्रोग्रामर्स यह इसलिए करते हैं ताकि डेट्स को आसानी से पढ़ा और स्टोर किया जा सके।

## How to: (कैसे करें:)
```c++
#include <iostream>
#include <iomanip>
#include <sstream>
#include <ctime>

std::string convertDateToString(const tm &date) {
    std::ostringstream stream;
    stream << std::put_time(&date, "%d-%m-%Y");
    return stream.str();
}

int main() {
    std::time_t t = std::time(nullptr);
    std::tm *datePtr = std::localtime(&t);
    
    std::string dateString = convertDateToString(*datePtr);
    std::cout << "आज की तारीख (स्ट्रिंग में): " << dateString << std::endl;
    
    return 0;
}
```

सैंपल आउटपुट:
```
आज की तारीख (स्ट्रिंग में): 31-12-2023
```

## Deep Dive (गहराई से जानकारी):
पहले कंप्यूटर सिस्टम्स में डेट और टाइम हैंडलिंग बहुत बेसिक थी। C++ में `<ctime>` हेडर फ़ाइल से डेट और टाइम का प्रबंधन किया जाता है, वहीं `<iomanip>` डेटा को फॉर्मेट करने के काम आती है। `std::put_time` एक मॉडर्न C++ फ़ंक्शन है जो `std::ostringstream` के साथ मिलकर डेट को चुनिंदा फॉर्मेट में स्ट्रिंग बनाने में सक्षम बनाता है। विकल्प के रूप में, बूस्ट लाइब्रेरी और C++20 से `std::format` जैसे अधिक आधुनिक समाधान भी मौजूद हैं, पर ये सभी कंपाइलर पर उपलब्ध नहीं हो सकते।

`std::put_time` और `strftime` फ़ंक्शन दशकों पुराने C फ़ंक्शन्स के मॉडर्न विकल्प हैं। सही फॉर्मेट स्पेसिफ़ायर ("`%d`", "`%m`", "`%Y`", इत्यादि) चुनना महत्वपूर्ण है, क्योंकि वे तारीख, महीने, साल को विभिन्न तरीकों में प्रस्तुत करने का काम करते हैं।

## See Also (और भी जानकारी):
- C++ `<ctime>` लाइब्रेरी डॉक्युमेंटेशन: https://en.cppreference.com/w/cpp/header/ctime
- C++ `<iomanip>` लाइब्रेरी डॉक्युमेंटेशन: https://en.cppreference.com/w/cpp/header/iomanip
- `std::put_time` के बारे में जानकारी: https://en.cppreference.com/w/cpp/io/manip/put_time
- C++20 `std::format`: https://en.cppreference.com/w/cpp/utility/format
