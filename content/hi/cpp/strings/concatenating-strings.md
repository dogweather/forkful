---
date: 2024-01-20 17:34:46.725170-07:00
description: "String concatenation \u0915\u093E \u092E\u0924\u0932\u092C \u0939\u094B\
  \u0924\u093E \u0939\u0948 \u0926\u094B \u092F\u093E \u0926\u094B \u0938\u0947 \u091C\
  \u094D\u092F\u093E\u0926\u093E strings \u0915\u094B \u090F\u0915 \u0938\u093E\u0925\
  \ \u091C\u094B\u0921\u093C\u0928\u093E. Programmers \u0907\u0938\u0947 \u0907\u0938\
  \u0932\u093F\u090F \u0915\u0930\u0924\u0947 \u0939\u0948\u0902 \u0915\u094D\u092F\
  \u094B\u0902\u0915\u093F \u0915\u0908 \u092C\u093E\u0930 output \u092E\u0947\u0902\
  \ information \u0915\u094B\u2026"
lastmod: '2024-03-13T22:44:52.830708-06:00'
model: gpt-4-1106-preview
summary: "String concatenation \u0915\u093E \u092E\u0924\u0932\u092C \u0939\u094B\u0924\
  \u093E \u0939\u0948 \u0926\u094B \u092F\u093E \u0926\u094B \u0938\u0947 \u091C\u094D\
  \u092F\u093E\u0926\u093E strings \u0915\u094B \u090F\u0915 \u0938\u093E\u0925 \u091C\
  \u094B\u0921\u093C\u0928\u093E."
title: "\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0915\u094B \u091C\u094B\
  \u0921\u093C\u0928\u093E"
weight: 3
---

## How to: (कैसे करें:)
C++ में strings को concatenate करना बहुत सीधा है. चलिए कुछ examples देखते हैं:

```cpp
#include <iostream>
#include <string> // C++ Standard Library का string class include करना जरूरी है

int main() {
    std::string hello = "नमस्ते";
    std::string world = " दुनिया";
    
    // '+' operator का इस्तेमाल करके concatenate करना
    std::string greeting = hello + world;
    std::cout << greeting << std::endl; // "नमस्ते दुनिया" output होगा

    // append() function का इस्तेमाल करें
    std::string completeGreeting = hello;
    completeGreeting.append(world);
    std::cout << completeGreeting << std::endl; // "नमस्ते दुनिया" output होगा

    return 0;
}
```

## Deep Dive (गहराई में जानकारी):
String concatenation का concept वास्तव में काफी पुराना है और यह शुरुआती programming languages से ही मौजूद है. C++ में strings को handling करने के कई तरीके हैं:

1. Historical context: C-style strings (`char` arrays) का इस्तेमाल पुराने C++ या C में होता था, जहाँ strings को concatenate करने के लिए `strcat` जैसे functions का प्रयोग होता था.

2. Alternatives: Modern C++ में, `std::string` class के साथ `+`, `+=` operators और `append()` method का उपयोग करना ज्यादा सुविधाजनक और सेफ है.

3. Implementation details: `std::string` पर operations perform करते वक्त, memory management और efficiency का ख्याल रखा जाता है. `append()` मेथड एक्सिस्टिंग string के अंत में डायरेक्टली जोड़ता है, जिससे performance बेहतर हो सकती है.

## See Also (संबंधित जानकारी):
- C++ `std::string` के बारे में और पढ़ें: [cppreference.com](https://en.cppreference.com/w/cpp/string/basic_string)
- String handling in C++: [cplusplus.com](http://www.cplusplus.com/reference/string/string/)
- C-style strings के बारे में और जानें: [cprogramming.com](https://www.cprogramming.com/tutorial/string.html)
