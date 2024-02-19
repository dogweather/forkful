---
aliases:
- /hi/cpp/interpolating-a-string/
date: 2024-01-20 17:50:41.785867-07:00
description: "String interpolation \u092E\u0947\u0902 \u0939\u092E variables \u0914\
  \u0930 expressions \u0915\u094B straightway strings \u0915\u0947 \u0905\u0902\u0926\
  \u0930 mix \u0915\u0930\u0924\u0947 \u0939\u0948\u0902, \u091C\u093F\u0938\u0938\
  \u0947 \u0915\u094B\u0921 read \u0915\u0930\u0928\u093E \u0914\u0930 write \u0915\
  \u0930\u0928\u093E \u0926\u094B\u0928\u094B\u0902 \u0906\u0938\u093E\u0928 \u0939\
  \u094B \u091C\u093E\u0924\u093E \u0939\u0948\u0964\u2026"
lastmod: 2024-02-18 23:09:03.864031
model: gpt-4-1106-preview
summary: "String interpolation \u092E\u0947\u0902 \u0939\u092E variables \u0914\u0930\
  \ expressions \u0915\u094B straightway strings \u0915\u0947 \u0905\u0902\u0926\u0930\
  \ mix \u0915\u0930\u0924\u0947 \u0939\u0948\u0902, \u091C\u093F\u0938\u0938\u0947\
  \ \u0915\u094B\u0921 read \u0915\u0930\u0928\u093E \u0914\u0930 write \u0915\u0930\
  \u0928\u093E \u0926\u094B\u0928\u094B\u0902 \u0906\u0938\u093E\u0928 \u0939\u094B\
  \ \u091C\u093E\u0924\u093E \u0939\u0948\u0964\u2026"
title: "\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0907\u0902\u091F\u0930\u092A\
  \u094B\u0932\u0947\u0936\u0928"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
String interpolation में हम variables और expressions को straightway strings के अंदर mix करते हैं, जिससे कोड read करना और write करना दोनों आसान हो जाता है। Programmers इसे use करते हैं क्योंकि यह कोड को और भी clean और maintainable बनाता है।

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
