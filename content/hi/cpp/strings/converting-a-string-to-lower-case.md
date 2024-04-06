---
date: 2024-01-20 17:38:53.997156-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) \u0909\u0926\
  \u093E\u0939\u0930\u0923 \u0915\u093E \u0906\u0909\u091F\u092A\u0941\u091F: `\u0928\
  \u092E\u0938\u094D\u0924\u0947 world!`."
lastmod: '2024-04-05T21:53:54.787262-06:00'
model: gpt-4-1106-preview
summary: "(\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) \u0909\u0926\u093E\u0939\
  \u0930\u0923 \u0915\u093E \u0906\u0909\u091F\u092A\u0941\u091F."
title: "\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0915\u094B \u091B\u094B\
  \u091F\u0947 \u0905\u0915\u094D\u0937\u0930\u094B\u0902 \u092E\u0947\u0902 \u092A\
  \u0930\u093F\u0935\u0930\u094D\u0924\u093F\u0924 \u0915\u0930\u0928\u093E"
weight: 4
---

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
