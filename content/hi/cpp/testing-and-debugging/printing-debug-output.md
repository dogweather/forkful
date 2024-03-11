---
date: 2024-01-20 17:52:33.346412-07:00
description: "Debug output \u092E\u0924\u0932\u092C \u0915\u094B\u0921 \u0938\u0947\
  \ \u0905\u0938\u094D\u0925\u093E\u092F\u0940 \u0938\u0902\u0926\u0947\u0936 \u092A\
  \u094D\u0930\u093F\u0902\u091F \u0915\u0930\u0928\u093E \u091C\u094B \u092A\u094D\
  \u0930\u094B\u0917\u094D\u0930\u093E\u092E \u0915\u0947 \u091A\u0932\u0928\u0947\
  \ \u0915\u093E \u0938\u093F\u0932\u0938\u093F\u0932\u093E \u0914\u0930 \u0909\u0938\
  \u0915\u0940 \u0938\u094D\u0925\u093F\u0924\u093F \u092C\u0924\u093E\u0924\u093E\
  \ \u0939\u0948\u0964 \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930\
  \u094D\u0938 \u0907\u0938\u0947 \u0907\u0938\u0932\u093F\u090F \u0915\u0930\u0924\
  \u0947 \u0939\u0948\u0902 \u0924\u093E\u0915\u093F \u0935\u0947 \u0915\u094B\u0921\
  \ \u0915\u0940\u2026"
lastmod: '2024-03-11T00:14:26.787906-06:00'
model: gpt-4-1106-preview
summary: "Debug output \u092E\u0924\u0932\u092C \u0915\u094B\u0921 \u0938\u0947 \u0905\
  \u0938\u094D\u0925\u093E\u092F\u0940 \u0938\u0902\u0926\u0947\u0936 \u092A\u094D\
  \u0930\u093F\u0902\u091F \u0915\u0930\u0928\u093E \u091C\u094B \u092A\u094D\u0930\
  \u094B\u0917\u094D\u0930\u093E\u092E \u0915\u0947 \u091A\u0932\u0928\u0947 \u0915\
  \u093E \u0938\u093F\u0932\u0938\u093F\u0932\u093E \u0914\u0930 \u0909\u0938\u0915\
  \u0940 \u0938\u094D\u0925\u093F\u0924\u093F \u092C\u0924\u093E\u0924\u093E \u0939\
  \u0948\u0964 \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930\u094D\u0938\
  \ \u0907\u0938\u0947 \u0907\u0938\u0932\u093F\u090F \u0915\u0930\u0924\u0947 \u0939\
  \u0948\u0902 \u0924\u093E\u0915\u093F \u0935\u0947 \u0915\u094B\u0921 \u0915\u0940\
  \u2026"
title: "\u0921\u0940\u092C\u0917 \u0906\u0909\u091F\u092A\u0941\u091F \u092A\u094D\
  \u0930\u093F\u0902\u091F \u0915\u0930\u0928\u093E"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
Debug output मतलब कोड से अस्थायी संदेश प्रिंट करना जो प्रोग्राम के चलने का सिलसिला और उसकी स्थिति बताता है। प्रोग्रामर्स इसे इसलिए करते हैं ताकि वे कोड की गलतियों को आसानी से खोज पाएं और उन्हें ठीक कर सकें।

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
