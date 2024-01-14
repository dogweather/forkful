---
title:                "C++: स्ट्रिंगों को जोड़ना"
programming_language: "C++"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/concatenating-strings.md"
---

{{< edit_this_page >}}

## क्यों

हमारे पास कभी-कभी टेक्स्ट को कंकटनेट करने की जरूरत होती है। यह दो या दो से अधिक टेक्स्ट स्ट्रिंग्स को एक साथ जोड़कर एक बड़ा स्ट्रिंग बनाता है। यह एक उपयोगी तकनीक है जो क्रमश: किसी भी कार्य को करने के लिए स्ट्रिंग डेटा को बनाता है। इसके अलावा, आप आसानी से दो स्ट्रिंग्स को जोड़ सकते हैं और प्रिंट कर सकते हैं।

## कैसे करें

```C++
#include <iostream>
#include <string>
using namespace std;

int main()
{
  // दो स्ट्रिंग्स डिफाइन करें
  string firstString = "नमस्ते";
  string secondString = "दुनिया";
  
  // स्ट्रिंग्स को जोड़ें
  string combined = firstString + secondString;
  
  // प्रिंट करें
  cout << combined;
  
  return 0;
}
```
आउटपुट:
नमस्ते दुनिया

## गहराई में जाएं

स्ट्रिंग्स को जोड़ने के लिए, C++ में दो काम करने के लिए विभिन्न तरीके हैं। पहले, आप स्ट्रिंग्स को '+ ऑपरेटर' के द्वारा जोड़ सकते हैं। दूसरा तरीका, `append()` फ़ंक्शन का उपयोग करके स्ट्रिंग को संलग्न करना है। आपको स्ट्रिंग्स को `+=` ऑपरेटर के साथ भी जोड़ सकते हैं। स्ट्रिंग के साथ जोड़ने के बाद आप उसको प्रिंट भी कर सकते हैं।

## देखें भी

- [C++ स्ट्रिंग हैंडलिंग](https://www.geeksforgeeks.org/c-plus-plus/#Strings)
- [स्ट्रिंग के साथ काम करना](https://www.tutorialspoint.com/cplusplus/cpp_strings.htm)
- [C++ स्ट्रिंग क्लास के संदर्भ में](https://www.programiz.com/cpp-programming/string)