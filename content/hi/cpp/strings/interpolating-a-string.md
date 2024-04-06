---
date: 2024-01-20 17:50:41.785867-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) \u092A\u0939\
  \u0932\u0947 C++ \u092E\u0947\u0902 string interpolation \u0915\u093E direct support\
  \ \u0928\u0939\u0940\u0902 \u0925\u093E\u0964 \u0932\u0947\u0915\u093F\u0928 C++20\
  \ \u0915\u0947 \u0938\u093E\u0925 `std::format` function \u0906\u092F\u093E, \u091C\
  \u094B Python \u0915\u0947 string\u2026"
lastmod: '2024-04-05T22:51:07.495648-06:00'
model: gpt-4-1106-preview
summary: "(\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) \u092A\u0939\u0932\u0947\
  \ C++ \u092E\u0947\u0902 string interpolation \u0915\u093E direct support \u0928\
  \u0939\u0940\u0902 \u0925\u093E\u0964 \u0932\u0947\u0915\u093F\u0928 C++20 \u0915\
  \u0947 \u0938\u093E\u0925 `std::format` function \u0906\u092F\u093E, \u091C\u094B\
  \ Python \u0915\u0947 string formatting \u0938\u0947 inspire \u0939\u0948\u0964\
  \ Traditional methods \u092E\u0947\u0902 `+` operator \u0914\u0930 `std::stringstream`\
  \ \u091C\u0948\u0938\u0947 alternatives \u0925\u0947, \u091C\u094B \u0905\u092C\
  \ `std::format` \u0915\u0947 \u0906\u0928\u0947 \u0915\u0947 \u092C\u093E\u0926\
  \ \u0915\u092E use \u0939\u094B\u0924\u0947 \u0939\u0948\u0902\u0964 String interpolation\
  \ \u0915\u0940 internal implementation detail \u092E\u0947\u0902 format string \u092A\
  \u093E\u0930\u094D\u0938 \u0939\u094B\u0924\u0940 \u0939\u0948 \u0914\u0930 \u0909\
  \u0938\u0915\u0947 placeholders \u0915\u094B respective values \u0938\u0947 replace\
  \ \u0915\u093F\u092F\u093E \u091C\u093E\u0924\u093E \u0939\u0948\u0964 \u092F\u0939\
  \ considerably complex \u0939\u0948, \u0907\u0938\u0932\u093F\u090F `std::format`\
  \ \u0915\u093E \u092F\u0942\u095B \u0915\u0930\u0915\u0947 \u0939\u092E abstraction\
  \ \u0915\u093E benefit \u0932\u0947 \u0938\u0915\u0924\u0947 \u0939\u0948\u0902\u0964\
  ."
title: "\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0907\u0902\u091F\u0930\u092A\
  \u094B\u0932\u0947\u0936\u0928"
weight: 8
---

## How to: (कैसे करें:)
```cpp
#include <iostream>
#include <string>
using std::string;
using std::cout;

int main() {
    string name = "Raj";
    int age = 30;
    
    // C++20 से पहले हमने '+' का use किया होता
    // cout << "My name is " + name + " and I am " + std::to_string(age) + " years old.\n";
    
    // C++20 में std::format का use करके interpolate कर सकते हैं
    cout << std::format("My name is {} and I am {} years old.\n", name, age);
    
    return 0;
}
```
Sample Output:
```
My name is Raj and I am 30 years old.
```

## Deep Dive (और गहराई में):
पहले C++ में string interpolation का direct support नहीं था। लेकिन C++20 के साथ `std::format` function आया, जो Python के string formatting से inspire है। Traditional methods में `+` operator और `std::stringstream` जैसे alternatives थे, जो अब `std::format` के आने के बाद कम use होते हैं।

String interpolation की internal implementation detail में format string पार्स होती है और उसके placeholders को respective values से replace किया जाता है। यह considerably complex है, इसलिए `std::format` का यूज़ करके हम abstraction का benefit ले सकते हैं।

## See Also (और भी देखें):
- C++ के `std::format` की official documentation: [cppreference std::format](https://en.cppreference.com/w/cpp/utility/format)
- C++ String Streams Guide: [std::stringstream](https://www.cplusplus.com/reference/sstream/stringstream/)
