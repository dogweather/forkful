---
title:                "यादृच्छिक संख्याएँ उत्पन्न करना"
date:                  2024-01-20T17:48:41.446097-07:00
model:                 gpt-4-1106-preview
simple_title:         "यादृच्छिक संख्याएँ उत्पन्न करना"
programming_language: "C++"
category:             "C++"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
रैंडम नंबर जनरेशन यानी अनियमित संख्याएँ उत्पन्न करना। प्रोग्रामर्स गेम्स, सिमुलेशन्स, और सिक्योरिटी में अप्रत्याशितता लाने के लिए इसका इस्तेमाल करते हैं।

## How to: (कैसे करें:)
```C++
#include <iostream>
#include <random>

int main() {
    // रैंडम नंबर जेनरेटर इनिशियलाइज करें
    std::random_device rd;
    std::mt19937 gen(rd());
    std::uniform_int_distribution<> dis(1, 100);

    // एक रैंडम नंबर उत्पन्न करें और प्रिंट करें
    int random_number = dis(gen);
    std::cout << "Random Number: " << random_number << std::endl;

    return 0;
}
```
सैम्पल आउटपुट: `Random Number: 42`

## Deep Dive (गहराई में जानकारी):
रैंडम नंबर जनरेटर्स (RNGs) के कई प्रकार होते हैं। पुराने दिनों में, `<cstdlib>` की `rand()` मुख्य विकल्प था। आजकल, `<random>` लाइब्रेरी अधिक सटीक और विविधतापूर्ण RNGs प्रदान करती है, जिसमें `mt19937` एक मौजूदा पसंद है। `mt19937` एक मर्सेन ट्विस्टर जेनरेटर है जो कि एक गैर-अनुक्रमिक, उच्च-गुणवत्ता के रैंडम नंबर्स पैदा करता है। क्रिप्टोग्राफिकली सिक्योर रैंडम नंबर्स के लिए `std::random_device` का उपयोग एक और विकल्प है।

## See Also (और जानकारी के लिए):
- C++ `<random>` library documentation: https://en.cppreference.com/w/cpp/header/random
- `std::mt19937` information: https://en.cppreference.com/w/cpp/numeric/random/mersenne_twister_engine
- `std::random_device` information: https://en.cppreference.com/w/cpp/numeric/random/random_device
- Random number generation in modern C++: https://www.moderncpp.com/random/
