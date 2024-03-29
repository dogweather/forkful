---
date: 2024-01-26 03:39:32.770157-07:00
description: "\u090F\u0915 \u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0938\
  \u0947 \u0909\u0926\u094D\u0927\u0930\u0923 \u091A\u093F\u0939\u094D\u0928\u094B\
  \u0902 (\u0915\u094B\u091F\u094D\u0938) \u0915\u094B \u0939\u091F\u093E\u0928\u0947\
  \ \u0915\u093E \u092E\u0924\u0932\u092C \u0939\u0948 \u0939\u092E\u093E\u0930\u0947\
  \ \u092A\u093E\u0920 \u0915\u094B \u0918\u0947\u0930\u0928\u0947 \u0935\u093E\u0932\
  \u0947 \u0907\u0928 \u092A\u0930\u0947\u0936\u093E\u0928 \u0915\u0930\u0928\u0947\
  \ \u0935\u093E\u0932\u0947 \u0926\u094B\u0939\u0930\u0947 \u092F\u093E \u090F\u0915\
  \u0932 \u091A\u0930\u093F\u0924\u094D\u0930\u094B\u0902 (' \u092F\u093E \") \u0915\
  \u094B \u0939\u091F\u093E\u0928\u093E\u0964\u2026"
lastmod: '2024-03-13T22:44:52.824064-06:00'
model: gpt-4-0125-preview
summary: "\u090F\u0915 \u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0938\u0947\
  \ \u0909\u0926\u094D\u0927\u0930\u0923 \u091A\u093F\u0939\u094D\u0928\u094B\u0902\
  \ (\u0915\u094B\u091F\u094D\u0938) \u0915\u094B \u0939\u091F\u093E\u0928\u0947 \u0915\
  \u093E \u092E\u0924\u0932\u092C \u0939\u0948 \u0939\u092E\u093E\u0930\u0947 \u092A\
  \u093E\u0920 \u0915\u094B \u0918\u0947\u0930\u0928\u0947 \u0935\u093E\u0932\u0947\
  \ \u0907\u0928 \u092A\u0930\u0947\u0936\u093E\u0928 \u0915\u0930\u0928\u0947 \u0935\
  \u093E\u0932\u0947 \u0926\u094B\u0939\u0930\u0947 \u092F\u093E \u090F\u0915\u0932\
  \ \u091A\u0930\u093F\u0924\u094D\u0930\u094B\u0902 (' \u092F\u093E \") \u0915\u094B\
  \ \u0939\u091F\u093E\u0928\u093E\u0964\u2026"
title: "\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0938\u0947 \u0909\u0926\
  \u094D\u0927\u0930\u0923 \u091A\u093F\u0939\u094D\u0928 \u0939\u091F\u093E\u0928\
  \u093E"
---

{{< edit_this_page >}}

## क्या और क्यों?
एक स्ट्रिंग से उद्धरण चिह्नों (कोट्स) को हटाने का मतलब है हमारे पाठ को घेरने वाले इन परेशान करने वाले दोहरे या एकल चरित्रों (' या ") को हटाना। प्रोग्रामर्स अक्सर इनपुट को साफ करने, डेटाबेस में पाठ को स्टोर करने, या उद्धरण चिह्नों के अव्यवस्था के बिना स्ट्रिंग्स को आगे की प्रोसेसिंग के लिए तैयार करने के लिए ऐसा करते हैं।

## कैसे करें:
यहाँ C++ में उद्धरण चिह्नों को किनारे करने का एक सीधा तरीका है:

```cpp
#include <iostream>
#include <algorithm>

std::string remove_quotes(std::string input) {
    input.erase(std::remove(input.begin(), input.end(), '\"'), input.end());
    input.erase(std::remove(input.begin(), input.end(), '\''), input.end());
    return input;
}

int main() {
    std::string original = R"("Hello, 'World'!")";
    std::string no_quotes = remove_quotes(original);
    std::cout << no_quotes << std::endl;
    return 0;
}
```

इसे चलाएं, और आपको मिलेगा:

```
Hello, World!
```

वाह! उद्धरण चिह्न गायब हो गए।

## गहराई में जाएँ
कंप्यूटिंग के आरंभ से ही उद्धरण चिह्न एक पाठ समस्या रहे हैं। पुराने दिनों में, आपने प्रोग्रामर्स को उन उद्धरणों को फ़िल्टर करने के लिए प्रत्येक वर्ण के माध्यम से परिश्रमपूर्वक लूप बनाते हुए देखा होगा। आज, हमारे पास मानक टेम्पलेट लाइब्रेरी (STL) में `std::remove` है जो भारी उठाने का काम करता है।

विकल्प? निश्चित रूप से! आप `std::regex` का उपयोग करके उद्धरण चिह्नों को निशाना बनाने के लिए नियमित अभिव्यक्तियों (रेगुलर एक्सप्रेशन्स) का उपयोग कर सकते हैं, लेकिन यह साधारण कार्यों के लिए एक बड़े हथौड़े से नट तोड़ने जैसा है - शक्तिशाली, लेकिन सरल कार्यों के लिए अधिक हो सकता है। हाल के C++ स्वादों का पक्ष लेने वालों के लिए, आप `std::string_view` का प्रयोग कर सकते हैं नॉन-मॉडिफाइंग दृष्टिकोण के लिए।

कार्यान्वयन के लिहाज से, याद रखें कि `std::remove` वास्तव में कंटेनर से तत्वों को हटाता नहीं है; यह गैर-हटाए गए तत्वों को आगे ले जाता है और कंटेनर के नए अंत के पार एक इटरेटर लौटाता है। इसलिए हमें वांछित पूंछ को काटने के लिए `erase` विधि की आवश्यकता होती है।

## यह भी देखें
- C++ `std::remove` संदर्भ: [cppreference.com](https://en.cppreference.com/w/cpp/algorithm/remove)
- `std::string` मैनिपुलेशन पर अधिक: [cplusplus.com](http://www.cplusplus.com/reference/string/string/)
