---
title:                "स्ट्रिंग को कैपिटलाइज़ करना"
date:                  2024-01-19
html_title:           "C: स्ट्रिंग को कैपिटलाइज़ करना"
simple_title:         "स्ट्रिंग को कैपिटलाइज़ करना"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)

स्ट्रिंग कैपिटलाइज़ेशन यानी स्ट्रिंग के शुरुआती अक्षर को बड़ा (कैपिटल लेटर) करना। प्रोग्रामर्स इसे डेटा को औपचारिकता देने, उपयोगकर्ता-इंटरफेस में सुधार और पढ़ने में सहजता के लिए करते हैं।

## How to (कैसे करें):

```C++
#include <iostream>
#include <cctype>
#include <algorithm>

std::string capitalizeString(const std::string& str) {
    std::string capitalized = str;
    if (!capitalized.empty()) {
        capitalized[0] = std::toupper(capitalized[0]);
    }
    return capitalized;
}

int main() {
    std::string myStr = "namaste duniya!";
    std::string capitalizedStr = capitalizeString(myStr);

    std::cout << "Original String: " << myStr << std::endl;
    std::cout << "Capitalized String: " << capitalizedStr << std::endl;

    return 0;
}
```

आउटपुट:
```
Original String: namaste duniya!
Capitalized String: Namaste duniya!
```

## Deep Dive (गहन जानकारी)

जब हम स्ट्रिंग के हर शब्द को कैपिटलाइज़ करना चाहते हैं, तो इसे Title case कहा जाता है। C++ में इसके लिए सीधा फंक्शन नहीं होता, तो हमें मैन्युअली इसे कोड करना पड़ता है। पुराने ज़माने में स्ट्रिंग ऑपरेशन्स काफी धीमे थे क्योंकि कंप्यूटर्स की गति कम होती थी। आज, हम std::transform फंक्शन जैसे टूल्स का इस्तेमाल करके तेज़ी से स्ट्रिंग्स को मॉडिफाई कर सकते हैं।

शब्दों की शुरुआत को बड़ा करने की प्रक्रिया में, एक-एक करके सभी अक्षरों पर जाना पड़ता है। स्ट्रिंग की लेंथ जितनी ज्यादा होगी, प्रोसेसिंग टाइम उतना ही लंबा होगा। इसलिए अगर प्रदर्शन महत्वपूर्ण है, तो स्ट्रिंग को कम से कम टाइम्स में हैंडल करना चाहिए।

तकनिकी विस्तार में जाएं, तो जहां टाइम कम्प्लेक्सिटी और स्पेस कम्प्लेक्सिटी का मामला हो, वहां स्ट्रिंग कैपिटलाइजेशन के लिए कुछ अलग एल्गोरिदम्स भी इस्तेमाल कर सकते हैं।

## See Also (और जानकारी के लिए)

1. [cplusplus.com - std::toupper](http://www.cplusplus.com/reference/cctype/toupper/)
2. [cppreference.com - std::transform](https://en.cppreference.com/w/cpp/algorithm/transform)
