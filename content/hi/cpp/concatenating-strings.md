---
title:                "C++: स्ट्रिंग्स को जोड़ना"
simple_title:         "स्ट्रिंग्स को जोड़ना"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/concatenating-strings.md"
---

{{< edit_this_page >}}

## क्यों

कॉनकैटनेटिंग स्ट्रिंग्स में लिखने का सबसे आसान और सबसे प्रभावी तरीका है अपने दिए गए पाठ को जोड़ना और संयोजन करना। यह आपके कोड को अन्य प्रोग्रामों से अलग और स्पष्ट बनाता है।

## कैसे करें

```C++
#include <iostream>
using namespace std;

int main() {
    // दो स्ट्रिंग्स बनाएं
    string str1 = "नमस्ते";
    string str2 = "दुनिया";

    // स्ट्रिंग्स को कॉनकैटनेट करें
    string concatenation = str1 + " " + str2;

    // कंसोल पर प्रिंट करें
    cout << concatenation << endl;

    // आउटपुट: नमस्ते दुनिया

    return 0;
}
```

## गहराई में जाएं

कॉनकैटनेटिंग स्ट्रिंग्स एक बहुत ही साधारण लेकिन उपयोगी काम है। इसके अतिरिक्त, आप अपने पाठ के साथ स्थिर लगाने के लिए स्वरूपण विकल्प का भी उपयोग कर सकते हैं। साथ ही, आप अपनी स्ट्रिंग्स को अलग-अलग स्थानों पर संकलित करने के लिए `stringstream` का भी उपयोग कर सकते हैं। इसलिए, कॉनकैटनेटिंग स्ट्रिंग्स सी ++ में अपने पाठ को प्रबंधित करने का एक महत्वपूर्ण तरीका है।

## देखें भी

- [C++ Strings](https://www.geeksforgeeks.org/cpp-strings/)
- [C++ stringstream](https://www.geeksforgeeks.org/stringstream-c-applications/)
- [Codecademy's Introduction to Strings in C++](https://www.codecademy.com/courses/learn-c-plus-plus/lessons/cpp-strings/exercises/intro-to-strings)