---
title:                "स्ट्रिंग की लंबाई पता करना"
html_title:           "C++: स्ट्रिंग की लंबाई पता करना"
simple_title:         "स्ट्रिंग की लंबाई पता करना"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

स्ट्रिंग की लंबाई का पता लगाना से तात्पर्य है, स्ट्रिंग में कितने वर्ण होते हैं, उसकी गणना करना। प्रोग्रामर्स इसे इसलिए करते हैं ताकि वे डाटा को सही तरीके से संभाल सकें और प्रबंधित कर सकें।

## ऐसे करें:

```C++
#include <iostream>
#include <string>

int main() {
    std::string str = "हैलो, वर्ल्ड!";
    std::cout << "स्ट्रिंग की लंबाई: " << str.length() << std::endl;

    return 0;
}
```
उपरोक्त कोड की आउटपुट होगी:

```C++
स्ट्रिंग की लंबाई: 14
```

## गहराई में:

1. ऐतिहासिक संदर्भ - पुराने C++ संस्करणों में, डेवलपर्स आमतौर पर इतरेशन का उपयोग करके स्ट्रिंग लेंथ निर्धारित करते थे। लेकिन, आधुनिक C++ लाइब्रेरी की `string` यह कार्य बहुत ही सरल बना देती है।
   
2. विकल्प - आप `str.size()` भी उपयोग कर सकते हैं। यह `str.length()` की तरह काम करता है।

3. कार्यान्वयन विवरण - `string.length()` और `string.size()` दोनों O(1) काम करते हैं, यानि वे तुरंत ही परिणाम प्रदान करते हैं, न कि स्ट्रिंग के वर्णों के माध्यम से इतरेट करते हैं।

## इसके अलावा देखें:

C++ स्ट्रिंग: [http://www.cplusplus.com/reference/string/string/](http://www.cplusplus.com/reference/string/string/)

C++ स्ट्रिंग लेंथ: [http://www.cplusplus.com/reference/string/string/length/](http://www.cplusplus.com/reference/string/string/length/)