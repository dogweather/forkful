---
title:    "C++: स्टैंडर्ड एरर पर लिखना"
keywords: ["C++"]
---

{{< edit_this_page >}}

## क्यों

Standard error में लेखन क्यों आवश्यक होता है, इसके बारे में केवल 1-2 वाक्यों में समझाया जाएगा। 

## कैसे करें

"```C++
#include <iostream>

int main() {
    std::cerr << "यह standard error में लिखा हुआ टेक्स्ट है।" << std::endl;
    return 0;
}
```

जब हम C++ में कोडिंग करते हैं, तो हमारे द्वारा लिखा गया टेक्स्ट default रूप से standard output में दिखाई देता है। लेकिन कई बार हमें इस टेक्स्ट को standard error में लिखना होता है। तो हम उसे `std::cerr` object के साथ लिख सकते हैं और उसे `endl` के साथ खत्म कर सकते हैं। 

यहां ध्यान रखें कि standard error अलग तरह से स्क्रीन पर दिखाई देता है। यह कई बार standard output के साथ ऐसी जगह जुड़ा होता है जहां आप टेक्स्ट को साफ़ से देखने में असमर्थ होते हैं। इसलिए, standard error का उपयोग बहुत उपयोगी हो सकता है। 

## गहराई में जाएं

Standard error से जुड़े प्रश्नों और तकनीकी पहलुओं को यहां समझाया जाएगा। 

Standard error को `std::cerr` के साथ आप खाली स्थान चनयें और उसे लाइन बाउंटर के साथ प्रिंट कर सकते हैं। इसके अलावा, आप `strerror()` फ़ंक्शन का इस्तेमाल करके last error message को देख सकते हैं। अंतिम रूप से, आप `std::cerr` के साथ `<<` ऑपरेटर भी उपयोग करके अपने custom error messages को लिख सकते हैं। 

## और भी देखें

- [Standard Output vs Standard Error in C++](https://www.geeksforgeeks.org/standard-output-vs-standard-error-in-c-cpp/)
- [How to redirect error messages to a file in C++](https://www.programming-techniques.com/2013/01/c-error-handling-with-redirecting-stdcerr-stdcout-to-files-in-c.html)
- [Error Handling in C++](https://www.learncpp.com/cpp-tutorial/error-handling-in-c/)
- [Understanding Standard Streams](https://www.fbbva.es/microsites/museo-de-informa/informa_uk/tech/