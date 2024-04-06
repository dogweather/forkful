---
date: 2024-01-20 17:52:33.346412-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902?) Debug output\
  \ \u0915\u0940 \u091C\u0921\u093C\u0947\u0902 console-based \u092A\u094D\u0930\u094B\
  \u0917\u094D\u0930\u093E\u092E\u093F\u0902\u0917 \u092E\u0947\u0902 \u0939\u0948\
  \u0902, \u091C\u092C GUI \u0928\u0939\u0940\u0902 \u0925\u0947\u0964 Print statements\
  \ \u0915\u093E \u0907\u0938\u094D\u0924\u0947\u092E\u093E\u0932 \u0909\u0938 \u0938\
  \u092E\u092F \u0938\u0947 \u0939\u094B \u0930\u0939\u093E \u0939\u0948 \u0924\u093E\
  \u0915\u093F\u2026"
lastmod: '2024-04-05T22:51:07.520467-06:00'
model: gpt-4-1106-preview
summary: "(\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902?) Debug output \u0915\
  \u0940 \u091C\u0921\u093C\u0947\u0902 console-based \u092A\u094D\u0930\u094B\u0917\
  \u094D\u0930\u093E\u092E\u093F\u0902\u0917 \u092E\u0947\u0902 \u0939\u0948\u0902\
  , \u091C\u092C GUI \u0928\u0939\u0940\u0902 \u0925\u0947\u0964 Print statements\
  \ \u0915\u093E \u0907\u0938\u094D\u0924\u0947\u092E\u093E\u0932 \u0909\u0938 \u0938\
  \u092E\u092F \u0938\u0947 \u0939\u094B \u0930\u0939\u093E \u0939\u0948 \u0924\u093E\
  \u0915\u093F \u0921\u0947\u0935\u0932\u092A\u0930\u094D\u0938 \u0915\u094B \u092A\
  \u0924\u093E \u091A\u0932\u0947 \u0915\u093F \u092A\u094D\u0930\u094B\u0917\u094D\
  \u0930\u093E\u092E \u0915\u0948\u0938\u0947 \u091A\u0932 \u0930\u0939\u093E \u0939\
  \u0948\u0964 Alternatives \u092E\u0947\u0902 logging libraries \u091C\u0948\u0938\
  \u0947 \u0915\u0940 log4cpp \u092F\u093E Boost.Log \u0936\u093E\u092E\u093F\u0932\
  \ \u0939\u0948\u0902\u0964 \u092F\u0947 \u0932\u093E\u0907\u092C\u094D\u0930\u0947\
  \u0930\u0940\u091C \u091C\u093C\u094D\u092F\u093E\u0926\u093E control \u0926\u0947\
  \u0924\u0940 \u0939\u0948\u0902, \u091C\u0948\u0938\u0947 \u0915\u093F log levels\
  \ \u0914\u0930 message formatting\u0964 Implementation \u0915\u0940 \u092C\u093E\
  \u0924 \u0915\u0930\u0947\u0902 \u0924\u094B, `std::cout` straightforward \u0939\
  \u0948, \u0932\u0947\u0915\u093F\u0928 \u092A\u094D\u0930\u094B\u0921\u0915\u094D\
  \u0936\u0928 \u0915\u094B\u0921 \u092E\u0947\u0902 debug statements \u0915\u094B\
  \ #ifdef DEBUG \u091C\u0948\u0938\u0947 preprocessor directives \u0938\u0947 surround\
  \ \u0915\u0930\u0928\u093E \u0905\u091A\u094D\u091B\u093E \u092E\u093E\u0928\u093E\
  \ \u091C\u093E\u0924\u093E \u0939\u0948, \u091C\u093F\u0938\u0938\u0947 release\
  \ build \u092E\u0947\u0902 \u0935\u0947 \u0936\u093E\u092E\u093F\u0932 \u0928 \u0939\
  \u094B\u0902\u0964."
title: "\u0921\u0940\u092C\u0917 \u0906\u0909\u091F\u092A\u0941\u091F \u092A\u094D\
  \u0930\u093F\u0902\u091F \u0915\u0930\u0928\u093E"
weight: 33
---

## How to: (कैसे करें?)
```C++
#include <iostream>
using namespace std;

int main() {
    // सामान्य Debug संदेश
    cout << "Debug: Main function starts" << endl;
    
    int sum = 0;
    for (int i = 0; i <= 5; i++) {
        sum += i;
        // Debug संदेश के लिए loop के अंदर
        cout << "Debug: i = " << i << ", sum = " << sum << endl;
    }
    
    cout << "Final sum is: " << sum << endl; // सामान्य आउटपुट
    // Debug संदेश की पहचान आसानी से हो इसलिए "Debug:" का उपयोग
    return 0;
}
```
Sample Output:
```
Debug: Main function starts
Debug: i = 0, sum = 0
Debug: i = 1, sum = 1
Debug: i = 2, sum = 3
Debug: i = 3, sum = 6
Debug: i = 4, sum = 10
Debug: i = 5, sum = 15
Final sum is: 15
```

## Deep Dive (गहरी जानकारी)
Debug output की जड़ें console-based प्रोग्रामिंग में हैं, जब GUI नहीं थे। Print statements का इस्तेमाल उस समय से हो रहा है ताकि डेवलपर्स को पता चले कि प्रोग्राम कैसे चल रहा है।

Alternatives में logging libraries जैसे की log4cpp या Boost.Log शामिल हैं। ये लाइब्रेरीज ज़्यादा control देती हैं, जैसे कि log levels और message formatting।

Implementation की बात करें तो, `std::cout` straightforward है, लेकिन प्रोडक्शन कोड में debug statements को #ifdef DEBUG जैसे preprocessor directives से surround करना अच्छा माना जाता है, जिससे release build में वे शामिल न हों।

## See Also (और भी देखें)
- C++ Standard Library Documentation: http://www.cplusplus.com/reference/
- Logging in C++: https://en.cppreference.com/w/cpp/io
- Debugging techniques: https://www.internalpointers.com/post/beginner-s-look-smart-pointers-modern-c

उम्मीद है कि आपको C++ में debug output के बारे में संक्षिप्त जानकारी मिल गई होगी। इसी तरह के और भी मजेदार और उपयोगी C++ टिप्स के लिए बने रहिए।
