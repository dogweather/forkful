---
title:    "C++: स्ट्रिंग्स को जोड़ना"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/concatenating-strings.md"
---

{{< edit_this_page >}}

## क्यों

C++ में स्ट्रिंग्स को जोड़ना एक बहुत उपयोगी कौशल है। यह बहुत से प्रोग्रामिंग सिनारियों में काम आ सकता है, जैसे प्रिंटिंग स्ट्रिंग्स, फ़ाइल संग्रहण और यूआई तर्कों को अपडेट करना। यह बहुत सरल होता है और आपको बहुत समय बचाता है जब आप अनुक्रमित स्ट्रिंग्स को काम करना चाहते हैं।

## कैसे

```C++
// स्ट्रिंग्स को जोड़ना

#include <iostream>
#include <string>
using namespace std;

int main()
{
  string s1 = "मेरा";
  string s2 = "नाम";
  string s3 = s1 + " " + s2; // स्ट्रिंग्स को जोड़ें
  cout << s3 << endl; // मेरा नाम
  return 0;
}
```

## गहराई में जाएं

स्ट्रिंग्स को जोड़ने के लिए, C++ एक बहुत सरल कौशल बनाता है। स्ट्रिंग्स को `+` अपरेटर के माध्यम से जोड़ना ही रूचिकर होता है। आप अनेक स्ट्रिंग्स को एक बार में भी जोड़ सकते हैं, जैसे `s1 + " " + s2` उदाहरण में देखा गया है।

## देखें भी

- [C++ एक समान गुणक का उपयोग करने के लिए उत्कृष्ट तरीके](https://www.javatpoint.com/cpp-string-equal-operator-overloading)
- [C++ स्ट्रिंग्स समीकरण और जोड़ना](https://www.geeksforgeeks.org/cpp-program-to-find-concatenation-of-two-strings/)
- [C++ स्ट्रिंग्स के साथ काम करना](https://www.geeksforgeeks.org/cpp-programming-working-with-strings/)