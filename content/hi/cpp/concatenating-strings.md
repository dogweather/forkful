---
title:    "C++: स्ट्रिंग्स को जोड़ना"
keywords: ["C++"]
---

{{< edit_this_page >}}

## क्यों
विपरीत स्थिति हो सकती है जहां हमें अलग-अलग स्ट्रिंग्स को एक साथ जोड़ने की जरूरत पड़ सकती है, जैसे कि एक संख्या को फोर्मेट करने के लिए उसको दिन, महीना और साल से अलग करना। इसलिए, स्ट्रिंग को जोड़ना बहुत उपयोगी और आसान हो सकता है।

## कैसे
```C++
#include <iostream>
using namespace std;

int main() {
  // पहले स्ट्रिंग्स बनाएं
  string firstName = "जॉन";
  string lastName = "डो";
  
  // स्ट्रिंग को जोड़ें और उसे प्रिंट करें
  string fullName = firstName + " " + lastName;
  cout << fullName << endl;

  return 0;
}
```

आउटपुट:
```
जॉन डो
```

## गहराई मेज़बानी
स्ट्रिंग को जोड़ने के बहुत से तरीके हो सकते हैं, जैसे कि ```+=``` ऑपरेटर का इस्तेमाल करना और स्ट्रिंग्स को सीधे प्रिंट करना। भी सीखने के लिए इन लिंक्स पर जाएँ:

- [C++ स्ट्रिंग्ज़: जोड़ना](https://www.tutorialspoint.com/cplusplus/cpp_strings_concatenate.htm)
- [स्ट्रिंग में += ऑपरेटर का उपयोग](https://www.geeksforgeeks.org/c-string-class-operators-2/)
- [C++ में स्ट्रिंग फंक्शन्स](https://www.geeksforgeeks.org/c-string-class-and-its-applications/)

## देखें भी
- [C++ स्ट्रिंग प्रोसेसिंग के उपयोगी तरीके](https://www.codespeedy.com/cpp-string-processing-tricks/)
- [C++ में स्ट्रिंग्स का अनुक्रमणिका उपयोग कैसे करें](https://www.umlindiabuzz.com/cpp-indexing-of-strings/)
- [C++ स्ट्रिंग्स के साथ खेलना कैसा हो सकता है](https://www.techiedelight.com/string-manipulation-techniques-cpp/)