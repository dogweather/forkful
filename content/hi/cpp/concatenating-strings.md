---
title:                "स्ट्रिंग्स को जोड़ना"
html_title:           "C++: स्ट्रिंग्स को जोड़ना"
simple_title:         "स्ट्रिंग्स को जोड़ना"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/concatenating-strings.md"
---

{{< edit_this_page >}}

बहुत सारे प्रोग्रामर पूरे दिन कंप्यूटर पर व्यवस्थित तरीके से काम करते हैं। वे अपने कोड को आसानी से समझने और संशोधित करने के लिए अलग अलग टेक्निक्स का उपयोग करते हैं। एक ऐसा तकनीक है कंकटनेटिंग स्ट्रिंग्स जिसके बारे में हम आज बात करेंगे और जानेंगे कि इसका प्रोग्रामिंग में क्या उपयोग होता है।

## What & Why?
कंकटनेटिंग स्ट्रिंग्स क्या होता है? कंकटनेटिंग स्ट्रिंग्स एक प्रोग्रामिंग तकनीक है जिसमें हम एक से ज्यादा स्ट्रिंग्स को आपस में जोड़ते हैं। यह प्रोग्रामिंग में उपयोग होता है क्योंकि इससे हम आसानी से एक से ज्यादा डेटा को एक साथ प्रबंधित कर सकते हैं।

## How to:
```C++
#include <iostream> 
using namespace std;
  
int main() 
{   
    string sentence1 = "मेरा";
    string sentence2 = "नाम";
    string sentence3 = "हिन्दी में है।";    
    string fullSentence = sentence1 + " " + sentence2 + " " + sentence3;
    
    cout << fullSentence << endl;
    
    return 0; 
} 

// Output: मेरा नाम हिन्दी में है।
```

## Deep Dive:
कंकटनेटिंग स्ट्रिंग्स का इतिहास क्या है? इस तकनीक की शुरुआत से ही इसका उपयोग स्ट्रिंग्स को मेमोरी में आसानी से स्टोर करने के लिए किया जाता था। इसके अलावा, दूसरे तकनीक जैसे की एरे, स्ट्रक्चर्स और फंक्शन्स का भी इस्तेमाल किया जाता है। इसमें स्ट्रिंग्स को मेमोरी में संग्रहीत करने के लिए कुछ सुनिश्चित किए गए steps होते हैं जिससे प्रोग्राम का और efficient बनाया जा सकता है।

## See Also:
- [C++ Strings: concat() vs '+' operator](https://www.geeksforgeeks.org/c-strings-concat-vs-operator/)
- [String Concatenation in C++](https://www.programiz.com/cpp-programming/library-function/cplusplus-string-concat)
- [Why is string::append faster than operator+?](https://stackoverflow.com/questions/12540381/why-is-stringappend-faster-than-operator)