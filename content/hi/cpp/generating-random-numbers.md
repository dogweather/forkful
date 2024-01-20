---
title:                "यादृच्छिक संख्याओं का निर्माण"
html_title:           "Clojure: यादृच्छिक संख्याओं का निर्माण"
simple_title:         "यादृच्छिक संख्याओं का निर्माण"
programming_language: "C++"
category:             "C++"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
रैंडम नंबर उत्पादन से मतलब है एक ऐसा नंबर उत्पन्न करना, जिसका अगले नंबर से कोई संबंध नहीं होता। प्रोग्रामर इसे गेम्स, डाटा परीक्षण, और एन्क्रिप्शन में उपयोग करते हैं क्योंकि यह अनपेक्षित परिणाम प्रस्तुत करता है।

## कैसे करें:
C++ में आपको लाइब्रेरी  ```<random>``` का उपयोग करके रैंडम नंबर उत्पन्न करना होता है।

```C++
#include <iostream>
#include <random>

int main() {
    // रैंडम इंजन सेट करें
    std::default_random_engine eng;
    
    // यूनिफ़ॉर्म डिस्ट्रिब्यूशन टाइप सेट करें
    std::uniform_int_distribution<> distr(1, 100);
    
    // रैंडम नंबर प्रिंट करें
    std::cout << distr(eng) << "\n";

    return 0;
}
```

आउटपुट:
```C++
58
```

## गहराई से जानिए
रैंडम नंबर उत्पादन के लिए हमें पहले pseudorandom number generator (PRNGs) का उपयोग करना पड़ता था, जो वास्तव में पूरी तरह से यादृच्छिक नहीं थे। C++11 से, नई `<random>` लाइब्रेरी का पेशकश किया गया, जिसने यादृच्छिकता को बेहतर किया।

हालांकि, अगर आप को हरद्वेयर आधारित रैंडम नंबर की आवश्यकता होती है, तब आप लाइब्रेरी `<random>` के आधार पर std::random_device का उपयोग कर सकते हैं।

## विशेष जानकारी
- [C++ विकी: random प्रलेखन](http://en.cppreference.com/w/cpp/numeric/random)