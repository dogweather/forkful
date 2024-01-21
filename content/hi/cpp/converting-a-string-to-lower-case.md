---
title:                "स्ट्रिंग को छोटे अक्षरों में परिवर्तित करना"
date:                  2024-01-20T17:38:53.997156-07:00
model:                 gpt-4-1106-preview
simple_title:         "स्ट्रिंग को छोटे अक्षरों में परिवर्तित करना"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
स्ट्रिंग को लोअर केस में बदलने से मतलब है हर अक्षर को छोटे (लोअरकेस) अक्षरों में परिवर्तित करना। प्रोग्रामर्स यह इसलिए करते हैं ताकि स्ट्रिंग प्रोसेसिंग में सामान्यीकरण हो सके और तुलना करना आसान हो जाए।

## How to: (कैसे करें:)
```C++
#include <iostream>
#include <algorithm>
#include <cctype>

int main() {
    std::string str = "नमस्ते World!";
    std::transform(str.begin(), str.end(), str.begin(), 
    [](unsigned char c){ return std::tolower(c); });
    
    std::cout << str << std::endl;
    return 0;
}
```
उदाहरण का आउटपुट: `नमस्ते world!`

## Deep Dive (गहराई से जानकारी)
पहले के जमाने में स्ट्रिंग को manipulate करने के लिए C-style functions जैसे `tolower()` use होते थे, और स्ट्रिंग्स C arrays की तरह होती थीं। C++ में `std::string` class और algorithms library के आगमन के बाद, स्ट्रिंग ऑपरेशंस को और भी सरल और अधिक शक्तिशाली बनाया गया। `std::transform` एक standard algorithm है जिसका use करके हम आसानी से string characters को इटरेट कर सकते हैं और किसी भी प्रकार का बदलाव कर सकते हैं। इसके अलावा, `boost` और `locale` जैसी libraries भी हैं जो कि विभिन्न भाषाओं और एन्कोडिंग्स के साथ बेहतर काम करती हैं। हालांकि, जब विशेष characters का प्रयोग होता है, तो लोकलाइज़ेशन और एन्कोडिंग अधिक महत्वपूर्ण हो जाते हैं।

## See Also (और जानकारी)
- `std::tolower`: http://www.cplusplus.com/reference/cctype/tolower/
- `std::transform`: http://www.cplusplus.com/reference/algorithm/transform/
- `std::string`: http://www.cplusplus.com/reference/string/string/