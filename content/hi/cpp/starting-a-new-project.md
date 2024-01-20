---
title:                "नया प्रोजेक्ट शुरू करना"
html_title:           "C: नया प्रोजेक्ट शुरू करना"
simple_title:         "नया प्रोजेक्ट शुरू करना"
programming_language: "C++"
category:             "C++"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/starting-a-new-project.md"
---

{{< edit_this_page >}}

## क्या और क्यों? | What & Why?

नया प्रोजेक्ट शुरू करना मतलब एक नई सॉफ्टवेयर सृजन प्रक्रिया की आरंभ करना। किसी प्रोग्रामर के लिए यह महत्वपूर्ण होता है ताकि वह अपने विचारों और समाधानों को वास्तविकता में बदल सके।

## कैसे करें : | How to:
सबसे आम तरीका किसी नये C++ प्रोजेक्ट को स्टार्ट करने का `main.cpp` फ़ाइल के साथ होता है।

```C++
// main.cpp
#include <iostream>
  
int main() {
    std::cout << "नमस्ते, दुनिया!\n";
    return 0;
}
```

इस कोड ब्लॉक को कम्पाइल और चलाने पर आपको निम्नलिखित आउटपुट मिलेगा:

```C++
नमस्ते, दुनिया!
```

## गहरी जानकारी | Deep Dive

C++ पहली बार 1985 में भाषा और संगठनात्मक स्थनापत्ति से उभरी। इसे बदलने और विकसित करने के लिए अनेक विभिन्न तरीके हैं, जिनमें से कुछ IDEs(इंटीग्रेटेड डेवलपमेंट एन्वायर्नमेंट), बिल्ड सिस्टम, लाइब्रेरी, फ्रेमवर्क और पैकेज मैनेजर शामिल हैं। प्राय: इन सबका उपयोग प्रोजेक्ट की वैविध्यता, प्रभावीता और सुरक्षा को बढ़ाने के लिए किया जाता है। 

## देखें भी | See Also

1. [GCC (GNU Compiler Collection)](https://gcc.gnu.org/)
2. [Microsoft Visual Studio](https://visualstudio.microsoft.com/vs/)
3. [CMake](https://cmake.org/)
4. [Boost C++ Libraries](https://www.boost.org/)
5. [C++ Core Guidelines](https://isocpp.github.io/CppCoreGuidelines/CppCoreGuidelines)