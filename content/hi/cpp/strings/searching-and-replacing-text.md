---
date: 2024-01-20 17:58:02.400086-07:00
description: "\u092A\u093E\u0920\u094D\u092F \u0938\u093E\u092E\u0917\u094D\u0930\u0940\
  \ \u092E\u0947\u0902 \u0916\u094B\u091C\u0928\u093E \u0914\u0930 \u092C\u0926\u0932\
  \u0928\u093E \u092F\u093E\u0928\u0940 \u0915\u093F \u0915\u093F\u0938\u0940 \u0926\
  \u093F\u090F \u0917\u090F \u092A\u093E\u0920 \u092E\u0947\u0902 \u0935\u093F\u0936\
  \u0947\u0937 \u0936\u092C\u094D\u0926 \u092F\u093E \u0935\u093E\u0915\u094D\u092F\
  \ \u0915\u094B \u0916\u094B\u091C\u0928\u093E \u0914\u0930 \u092B\u093F\u0930 \u0909\
  \u0938\u0947 \u0926\u0942\u0938\u0930\u0947 \u092A\u093E\u0920 \u0938\u0947 \u092C\
  \u0926\u0932\u0928\u093E\u0964 \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\
  \u0930\u094D\u0938 \u092F\u0939 \u0915\u093E\u0930\u094D\u092F \u0921\u0947\u091F\
  \u093E \u0938\u0902\u0936\u094B\u0927\u0928\u2026"
lastmod: '2024-03-13T22:44:52.819421-06:00'
model: gpt-4-1106-preview
summary: "\u092A\u093E\u0920\u094D\u092F \u0938\u093E\u092E\u0917\u094D\u0930\u0940\
  \ \u092E\u0947\u0902 \u0916\u094B\u091C\u0928\u093E \u0914\u0930 \u092C\u0926\u0932\
  \u0928\u093E \u092F\u093E\u0928\u0940 \u0915\u093F \u0915\u093F\u0938\u0940 \u0926\
  \u093F\u090F \u0917\u090F \u092A\u093E\u0920 \u092E\u0947\u0902 \u0935\u093F\u0936\
  \u0947\u0937 \u0936\u092C\u094D\u0926 \u092F\u093E \u0935\u093E\u0915\u094D\u092F\
  \ \u0915\u094B \u0916\u094B\u091C\u0928\u093E \u0914\u0930 \u092B\u093F\u0930 \u0909\
  \u0938\u0947 \u0926\u0942\u0938\u0930\u0947 \u092A\u093E\u0920 \u0938\u0947 \u092C\
  \u0926\u0932\u0928\u093E\u0964 \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\
  \u0930\u094D\u0938 \u092F\u0939 \u0915\u093E\u0930\u094D\u092F \u0921\u0947\u091F\
  \u093E \u0938\u0902\u0936\u094B\u0927\u0928\u2026"
title: "\u092A\u093E\u0920 \u0916\u094B\u091C\u0928\u093E \u0914\u0930 \u092C\u0926\
  \u0932\u0928\u093E"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
पाठ्य सामग्री में खोजना और बदलना यानी कि किसी दिए गए पाठ में विशेष शब्द या वाक्य को खोजना और फिर उसे दूसरे पाठ से बदलना। प्रोग्रामर्स यह कार्य डेटा संशोधन और सुधारने के लिए करते हैं, ताकि वे आवश्यक परिवर्तन कर सकें और गलतियों को सही कर सकें।

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
