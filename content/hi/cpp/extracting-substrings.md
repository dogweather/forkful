---
title:                "उपस्करणों को निकालना"
html_title:           "C++: उपस्करणों को निकालना"
simple_title:         "उपस्करणों को निकालना"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/extracting-substrings.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

अक्सर हमें स्ट्रिंग के ऊपर कुछ विशेष काम करना होता है, जैसे कि उसमें से कुछ विशेष अक्षरों को निकालना। इसे हम स्ट्रिंग के उपस्थित अक्षरों को प्रच्छालित करके कर सकते हैं। इसका उदाहरण यह है कि आपको एक स्ट्रिंग में से उसकी पहचान और अन्य विशेषताओं को छोटा करना चाहिए। कई बार, इसका उपयोग डेटा संगठन या स्ट्रिंग प्रोसेसिंग आदि में किया जाता है।

## कैसे करें?

```C++
// Input string
string s = "Hello World";

// Extracting substring "World"
string substring = s.substr(6, 5); // substr(starting_index, length)

// Output
cout << substring; // World
```

## गहराई तक जाएं

इस तकनीक का प्रयोग स्ट्रिंग प्रोसेसिंग के विभिन्न तरीकों में किया जाता है। इसका प्रयोग लंबे स्ट्रिंग को छोटे पर्चों में टुकड़ों में विभाजित करने में भी किया जा सकता है। जबकि, इसके लिए अलग-अलग तरीकों का प्रयोग किया जा सकता है। उनमें से कुछ हैं `find()`, `find_first_of()`, और `substr()` जो स्ट्रिंग से उपस्थित सदस्यों को निकालने के लिए प्रयोग किए जाते हैं।

## इसे भी देखें

- [C++ Strings](https://www.geeksforgeeks.org/c-string-class-and-its-applications/)
- [C++ string::substr()](https://www.cplusplus.com/reference/string/string/substr/)