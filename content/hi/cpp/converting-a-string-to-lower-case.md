---
title:                "C++: स्ट्रिंग को लोअर केस में रूपांतरित करना"
programming_language: "C++"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## क्यों

अक्सर हमें ये जानने की जरूरत होती है कि स्ट्रिंग को लोअर केस में बदलना क्यों जरूरी है। शायद हमारे प्रोग्राम में उस स्ट्रिंग का उपयोग निर्दिष्ट प्रारूप में होना हो, या फिर हमें सबसे बड़ा स्ट्रिंग तुलनायोग्यता नंबर की तरह स्ट्रिंग का प्रयोग करना हो। यह वस्तुनिष्ठ बटिया स्ट्रिंग के साथ काम करने को भी अनुमति देता है। इस ब्लॉग पोस्ट में, हम स्ट्रिंग को लोअर केस में कॉन्वर्ट करने का तरीका सीखेंगे।

## कैसे करें

```C++
#include <iostream>
#include <string>
using namespace std;

int main() {
  // स्ट्रिंग बनाएं
  string str = "Hello, World!";
  // स्ट्रिंग को लोअर केस में कन्वर्ट करने के लिए transform मेथड का उपयोग करें
  transform(str.begin(), str.end(), str.begin(), ::tolower);
  // कन्वर्टेड स्ट्रिंग को प्रिंट करें
  cout << str; // Output: hello, world!
  return 0;
}
```

## गहराई में जाएं

स्ट्रिंग को लोअर केस में कन्वर्ट करने के लिए, हम अपने कोड में लाइब्रेरी `<string>` को इन्क्लूड करते हैं। फिर, `transform()` फ़ंक्शन का उपयोग करते हुए, हम स्ट्रिंग के प्रत्येक चर को बराबरिता की तकनीक का उपयोग कर सकते हैं और उसे `::tolower` पर लागू कर सकते हैं। यह चर लोअर केस में कन्वर्ट हो जाते हैं। अंत में, स्ट्रिंग को प्रिंट करने के लिए `cout` का उपयोग कर सकते हैं। इस तरह, हम स्ट्रिंग को लोअर केस में कन्वर्ट कर सकते हैं।

## देखें भी

- [C++ String Functions](https://www.geeksforgeeks.org/cpp-string-functions-set-1/)
- [C++ Transform Function](https://www.cplusplus.com/reference/algorithm/