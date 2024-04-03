---
date: 2024-01-20 17:45:30.809979-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) ."
lastmod: '2024-03-13T22:44:52.825752-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u0938\u092C\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917\u094D\u0938 \u0928\
  \u093F\u0915\u093E\u0932\u0928\u093E"
weight: 6
---

## How to: (कैसे करें:)
```C++
#include <iostream>
#include <string>

int main() {
    // मूल स्ट्रिंग
    std::string fullString = "नमस्ते, मैं C++ सीख रहा हूँ!";

    // सबस्ट्रिंग निकालना
    std::string greeting = fullString.substr(0, 7); // "नमस्ते," को निकालें

    // रिजल्ट दिखाना
    std::cout << greeting << std::endl; // "नमस्ते," प्रिंट होगा

    // एक और उदाहरण
    std::string learning = fullString.substr(9); // "मैं C++ सीख रहा हूँ!" को निकालें
    std::cout << learning << std::endl; // "मैं C++ सीख रहा हूँ!" प्रिंट होगा
    return 0;
}
```
सैंपल आउटपुट:
```
नमस्ते,
मैं C++ सीख रहा हूँ!
```

## Deep Dive (गहराई से समझिए)
C++ में `std::string` क्लास का `.substr()` फंक्शन काफी पुराना है और सबस्ट्रिंग निकालने का बुनियादी तरीका बन चुका है। इसके अलावा, आप अन्य लाइब्रेरीज जैसे `boost` का भी इस्तेमाल कर सकते हैं, जो और भी फ्लेक्सिबल सोल्यूशंस प्रदान करते हैं। `.substr()` फंक्शन को दो पैरामीटर्स के साथ बुलाया जाता है: पहला है स्टार्टिंग इंडेक्स और दूसरा है लेंथ। अगर लेंथ प्रोवाइड नहीं की जाती, तो यह रेस्ट ऑफ द स्ट्रिंग को निकाल लेता है।

## See Also (और देखिए)
- C++ [`std::string` documentation](https://en.cppreference.com/w/cpp/string/basic_string)
- [`boost` library](https://www.boost.org/)
- [String manipulation techniques in C++](https://www.cplusplus.com/articles/string_manipulation/)
