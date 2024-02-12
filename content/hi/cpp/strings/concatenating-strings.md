---
title:                "स्ट्रिंग को जोड़ना"
aliases:
- /hi/cpp/concatenating-strings.md
date:                  2024-01-20T17:34:46.725170-07:00
model:                 gpt-4-1106-preview
simple_title:         "स्ट्रिंग को जोड़ना"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
String concatenation का मतलब होता है दो या दो से ज्यादा strings को एक साथ जोड़ना. Programmers इसे इसलिए करते हैं क्योंकि कई बार output में information को एक सीक्वेंस में दिखाना होता है, या फिर बड़ी strings के पार्ट्स को मैनेज करना होता है.

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
