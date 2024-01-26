---
title:                "परीक्षण लिखना"
html_title:           "Arduino: परीक्षण लिखना"
simple_title:         "परीक्षण लिखना"
programming_language: "C++"
category:             "C++"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/writing-tests.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

टेस्ट लिखना यानी कोड की जांच करना ताकि बग्स को दूर किया जा सके। प्रोग्रामर्स इसे इसलिए करते हैं ताकि उनका सॉफ्टवेयर विश्वसनीय और दुरुस्त रहे।

## कैसे:

```C++
#include <cassert>

int add(int a, int b) {
    return a + b;
}

int main() {
    assert(add(2, 2) == 4);
    assert(add(0, 0) == 0);
    assert(add(-1, 1) == 0);

    return 0;
}
```
ऊपर दिया गया कोड साधारण यूनिट टेस्ट्स का उदाहरण है। `assert` फंक्शन का इस्तेमाल कर के हमने जांच की है कि `add` फंक्शन सही आउटपुट दे रहा है या नहीं। 

## गहन जानकारी:

टेस्टिंग का इतिहास काफी पुराना है, लेकिन टेस्ट-ड्रिवेन डेवलपमेंट (TDD) जैसी प्रक्रियाएं 2000 की शुरुआत में लोकप्रिय हुईं। वैकल्पिक रूपों में मॉक ऑब्जेक्ट्स, स्टब्स और फेक्स आते हैं। C++ में टेस्टिंग के लिए Google Test और Boost.Test जैसे फ्रेमवर्क्स भी उपलब्ध हैं, जो परीक्षण को और आसान और प्रभावी बनाते हैं।

## सम्बंधित सूत्र:

- Google Test (gtest): https://github.com/google/googletest
- Boost.Test: https://www.boost.org/doc/libs/release/libs/test/
- Unit Testing Tutorial: https://www.tutorialspoint.com/software_testing_dictionary/unit_testing.htm
- C++ Reference for `cassert`: https://en.cppreference.com/w/cpp/error/assert
