---
title:                "पाठ खोजना और बदलना"
aliases: - /hi/cpp/searching-and-replacing-text.md
date:                  2024-01-20T17:58:02.400086-07:00
model:                 gpt-4-1106-preview
simple_title:         "पाठ खोजना और बदलना"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/searching-and-replacing-text.md"
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
