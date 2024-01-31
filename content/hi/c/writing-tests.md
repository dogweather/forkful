---
title:                "परीक्षण लिखना"
date:                  2024-01-19
simple_title:         "परीक्षण लिखना"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/writing-tests.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

परीक्षण लेखन, यानी टेस्टिंग, यह जांचने की प्रक्रिया है कि आपका कोड सही ढंग से काम कर रहा है या नहीं। प्रोग्रामर्स इसका उपयोग कोड की गुणवत्ता एवं स्थिरता को सुनिश्चित करने के लिए करते हैं।

## कैसे करें:

```C
#include <stdio.h>
#include <assert.h>

// एक सरल फंक्शन जो दो संख्या का योग देता है
int add(int a, int b) {
    return a + b;
}

void test_add() {
    assert(add(2, 2) == 4); // यह टेस्ट सही है
    assert(add(-1, 1) == 0); // यह भी सही है
    // assert(add(2, 2) == 5); // यह टेस्ट गलत है और प्रोग्राम को असमर्थ कर देगा
}

int main() {
    test_add(); // टेस्ट फंक्शन को कॉल करें
    printf("सभी टेस्ट पास हो चुके हैं!\n");
    return 0;
}
```
सैम्पल आउटपुट:
```
सभी टेस्ट पास हो चुके हैं!
```

## गहन जानकारी:

परीक्षण लेखन की शुरुआत विकास प्रक्रिया के आरंभिक दिनों मे ही की गयी थी ताकि समय के साथ त्रुटियों का पता चल सके और उन्हें सही किया जा सके। एक वैकल्पिक तरीका है ऑटोमेटेड टेस्टिंग जैसे कि यूनिट टेस्ट्स, इंटीग्रेशन टेस्ट्स, और सिस्टम टेस्ट्स। इन टेस्ट्स को C भाषा में विभिन्न टेस्ट फ्रेमवर्क, जैसे कि Check, Unity, आदि के सहायता से लागू किया जा सकता है।

## देखें यह भी:

- C प्रोग्रामिंग की यूनिट टेस्टिंग फ्रेमवर्क्स: [Check](https://libcheck.github.io/check/), [Unity](https://www.throwtheswitch.org/unity)
- टेस्ट ड्रिवन डेवलपमेंट(TDD): [Wiki TDD](https://en.wikipedia.org/wiki/Test-driven_development)
- सी भाषा में एडवांस टेस्टिंग के बारे में आर्टिकल: [Advanced C Testing](https://www.codeproject.com/Articles/383185/Unit-testing-in-C-and-Cplusplus)
