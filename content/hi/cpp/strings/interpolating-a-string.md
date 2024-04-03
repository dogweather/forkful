---
date: 2024-01-20 17:50:41.785867-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) ."
lastmod: '2024-03-13T22:44:52.820796-06:00'
model: gpt-4-1106-preview
summary: .
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
