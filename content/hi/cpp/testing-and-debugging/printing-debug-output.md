---
date: 2024-01-20 17:52:33.346412-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902?) ."
lastmod: '2024-03-13T22:44:52.848847-06:00'
model: gpt-4-1106-preview
summary: .
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
