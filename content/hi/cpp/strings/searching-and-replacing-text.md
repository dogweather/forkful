---
date: 2024-01-20 17:58:02.400086-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) \u092A\u093E\
  \u0920\u094D\u092F \u0938\u093E\u092E\u0917\u094D\u0930\u0940 \u092E\u0947\u0902\
  \ \u0916\u094B\u091C \u0914\u0930 \u092C\u0926\u0932\u093E\u0935 \u0915\u0940 \u0905\
  \u0935\u0927\u093E\u0930\u0923\u093E \u092A\u0941\u0930\u093E\u0928\u0940 \u0939\
  \u0948, \u0914\u0930 \u092F\u0939 \u092A\u0939\u0932\u0940 \u092C\u093E\u0930 \u090F\
  \u0921\u093F\u091F\u093F\u0902\u0917 \u0938\u0949\u092B\u094D\u091F\u0935\u0947\u092F\
  \u0930 \u092E\u0947\u0902 \u0926\u093F\u0916\u093E\u0908 \u0926\u0940 \u0925\u0940\
  \u0964 \u0905\u0932\u094D\u091F\u0930\u0928\u0947\u091F\u093F\u0935\u094D\u0938\
  \ \u092E\u0947\u0902 \u0930\u0947\u0917\u0941\u0932\u0930\u2026"
lastmod: '2024-04-05T22:51:07.494090-06:00'
model: gpt-4-1106-preview
summary: "(\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) \u092A\u093E\u0920\u094D\
  \u092F \u0938\u093E\u092E\u0917\u094D\u0930\u0940 \u092E\u0947\u0902 \u0916\u094B\
  \u091C \u0914\u0930 \u092C\u0926\u0932\u093E\u0935 \u0915\u0940 \u0905\u0935\u0927\
  \u093E\u0930\u0923\u093E \u092A\u0941\u0930\u093E\u0928\u0940 \u0939\u0948, \u0914\
  \u0930 \u092F\u0939 \u092A\u0939\u0932\u0940 \u092C\u093E\u0930 \u090F\u0921\u093F\
  \u091F\u093F\u0902\u0917 \u0938\u0949\u092B\u094D\u091F\u0935\u0947\u092F\u0930\
  \ \u092E\u0947\u0902 \u0926\u093F\u0916\u093E\u0908 \u0926\u0940 \u0925\u0940\u0964\
  \ \u0905\u0932\u094D\u091F\u0930\u0928\u0947\u091F\u093F\u0935\u094D\u0938 \u092E\
  \u0947\u0902 \u0930\u0947\u0917\u0941\u0932\u0930 \u090F\u0915\u094D\u0938\u092A\
  \u094D\u0930\u0947\u0936\u0928\u094D\u0938 (Regular Expressions) \u0914\u0930 \u091F\
  \u0947\u0915\u094D\u0938\u094D\u091F \u092A\u094D\u0930\u094B\u0938\u0947\u0938\u093F\
  \u0902\u0917 \u091F\u0942\u0932\u094D\u0938 \u0936\u093E\u092E\u093F\u0932 \u0939\
  \u0948\u0902 \u091C\u0948\u0938\u0947 `sed` \u0914\u0930 `awk` \u091C\u094B UNIX\
  \ \u091C\u0948\u0938\u0947 \u0938\u093F\u0938\u094D\u091F\u092E\u094D\u0938 \u092E\
  \u0947\u0902 \u0909\u092A\u092F\u094B\u0917 \u0939\u094B\u0924\u0947 \u0939\u0948\
  \u0902\u0964 C++ \u092E\u0947\u0902 `std::string` \u0915\u093E `find` \u0914\u0930\
  \ `replace` \u092B\u0902\u0915\u094D\u0936\u0928\u094D\u0938 \u0907\u0938\u094D\u0924\
  \u0947\u092E\u093E\u0932 \u0915\u0930\u0924\u0947 \u0939\u0941\u090F \u0939\u092E\
  \ \u0906\u0938\u093E\u0928\u0940 \u0938\u0947 \u0916\u094B\u091C \u0914\u0930 \u092C\
  \u0926\u0932 \u0938\u0915\u0924\u0947 \u0939\u0948\u0902\u0964 `find` \u092E\u0947\
  \u0925\u0921 \u0916\u093E\u0938 \u092A\u093E\u0920 \u0915\u093E \u0907\u0902\u0921\
  \u0947\u0915\u094D\u0938 \u0932\u094C\u091F\u093E\u0924\u0940 \u0939\u0948, \u091C\
  \u092C\u0915\u093F `replace` \u0907\u0938 \u0907\u0902\u0921\u0947\u0915\u094D\u0938\
  \ \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\u0915\u0947 \u0928\u093F\
  \u0936\u094D\u091A\u093F\u0924 \u0938\u094D\u0925\u093E\u0928 \u092A\u0930 \u092A\
  \u093E\u0920 \u0915\u094B \u092C\u0926\u0932 \u0926\u0947\u0924\u0940 \u0939\u0948\
  \u0964."
title: "\u092A\u093E\u0920 \u0916\u094B\u091C\u0928\u093E \u0914\u0930 \u092C\u0926\
  \u0932\u0928\u093E"
weight: 10
---

## How to: (कैसे करें:)
```C++
#include <iostream>
#include <string>
#include <algorithm>

int main() {
    std::string text = "हम सभी प्रोग्रामिंग सीख रहे हैं।";
    std::string to_search = "प्रोग्रामिंग";
    std::string to_replace = "C++ भाषा";
    
    std::size_t pos = text.find(to_search);
    if (pos != std::string::npos) {
        text.replace(pos, to_search.length(), to_replace);
    }
    
    std::cout << text << std::endl; // आउटपुट: हम सभी C++ भाषा सीख रहे हैं।
    return 0;
}
```

## Deep Dive (गहराई में जानकारी):
पाठ्य सामग्री में खोज और बदलाव की अवधारणा पुरानी है, और यह पहली बार एडिटिंग सॉफ्टवेयर में दिखाई दी थी। अल्टरनेटिव्स में रेगुलर एक्सप्रेशन्स (Regular Expressions) और टेक्स्ट प्रोसेसिंग टूल्स शामिल हैं जैसे `sed` और `awk` जो UNIX जैसे सिस्टम्स में उपयोग होते हैं। C++ में `std::string` का `find` और `replace` फंक्शन्स इस्तेमाल करते हुए हम आसानी से खोज और बदल सकते हैं। `find` मेथड खास पाठ का इंडेक्स लौटाती है, जबकि `replace` इस इंडेक्स का उपयोग करके निश्चित स्थान पर पाठ को बदल देती है।

## See Also (देखें भी):
- C++ `std::string` documentation: https://en.cppreference.com/w/cpp/string/basic_string
- Regular expressions in C++: https://en.cppreference.com/w/cpp/regex
- `sed` tutorial: https://www.gnu.org/software/sed/manual/sed.html
- `awk` programming language: https://www.gnu.org/software/gawk/manual/gawk.html
