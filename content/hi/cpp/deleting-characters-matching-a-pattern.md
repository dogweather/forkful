---
title:                "C++: पैटर्न के समान वर्णों को हटाना"
simple_title:         "पैटर्न के समान वर्णों को हटाना"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## क्यों

कभी-कभी हमें एक पैटर्न से मिलती जुलती चरित्रों को हटाने की आवश्यकता होती है। इस ब्लॉग पोस्ट में, हम जानेंगे कि इसका होना क्यों ज़रूरी हो सकता है।

## कैसे करें

इस समस्या का हल करने के लिए, हम C++ में डेटा स्ट्रक्चर्स का इस्तेमाल कर सकते हैं। निम्नलिखित कोड ब्लॉक में हमने इस काम को कैसे करना है विस्तार से देखेंगे।

```C++
#include <iostream>
#include <string>

using namespace std;

int main() {

  string str = "Hello World!";
  
  // पैटर्न जो हटाना है
  string pattern = "l";
  
  // पैटर्न से मिलते जुलते चरित्रों को हटाने का प्रोसेस
  int pos = str.find(pattern);
  
  while (pos != string::npos) {
    str.erase(pos, 1);
    pos = str.find(pattern, pos);
  }
  
  // नए स्ट्रिंग को आउटपुट करें
  cout << str << endl;
  
  return 0;

}
```

आउटपुट: Heo Word!

जैसा कि आप देख सकते हैं, हमारे द्वारा निर्दिष्ट पैटर्न 'l' को हटा दिया गया है।

## गहराई में जाएं

इस प्रकार से क्वेश्चन को हटाने का बहुत ही अन्य प्रकार है जो हमारे पास हैं, जिनमें यह मजबूती से सामान्य नियम के साथ काम कर सकता है। सैम्पल अर्थव्यवस्था में, एक स्ट्रिंग के सांख्यिकीय स्तंभ में से सारे प्लस लाइन को हटाने का कोड है।

```C++
for (int i = 0; i < str.size(); i++) {
  if (str[i] == '+') {
    str.erase(i, 1);
  }
}
```

## देखें लिंक

- [C++ डाटा स्ट्रक्चर्स](https://www.geeksforgeeks.org/data-structures/)
- [string::find() फ़ंक्शन](https://www.cplusplus.com/reference/string/string/find/)