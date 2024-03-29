---
date: 2024-01-26 03:44:01.998615-07:00
description: "\u0938\u0902\u0916\u094D\u092F\u093E\u0913\u0902 \u0915\u094B \u0917\
  \u094B\u0932 \u0915\u0930\u0928\u0947 \u0915\u093E \u092E\u0924\u0932\u092C \u0939\
  \u0948 \u090F\u0915 \u092E\u0942\u0932\u094D\u092F \u0915\u094B \u0909\u0938\u0915\
  \u0947 \u0928\u091C\u0926\u0940\u0915\u0940 \u092A\u0942\u0930\u094D\u0923\u093E\
  \u0902\u0915 \u092F\u093E \u0928\u093F\u0930\u094D\u0926\u093F\u0937\u094D\u091F\
  \ \u0938\u091F\u0940\u0915\u0924\u093E \u0924\u0915 \u0938\u0902\u0936\u094B\u0927\
  \u093F\u0924 \u0915\u0930\u0928\u093E\u0964 \u0921\u0947\u0935\u0932\u092A\u0930\
  \u094D\u0938 \u0907\u0938\u0947 \u0938\u0930\u0932\u0940\u0915\u0930\u0923, \u0935\
  \u093E\u0938\u094D\u0924\u0935\u093F\u0915 \u0926\u0941\u0928\u093F\u092F\u093E\
  \ \u0915\u0940 \u0938\u0940\u092E\u093E\u0913\u0902 \u0915\u0947\u2026"
lastmod: '2024-03-13T22:44:52.835761-06:00'
model: gpt-4-0125-preview
summary: "\u0938\u0902\u0916\u094D\u092F\u093E\u0913\u0902 \u0915\u094B \u0917\u094B\
  \u0932 \u0915\u0930\u0928\u0947 \u0915\u093E \u092E\u0924\u0932\u092C \u0939\u0948\
  \ \u090F\u0915 \u092E\u0942\u0932\u094D\u092F \u0915\u094B \u0909\u0938\u0915\u0947\
  \ \u0928\u091C\u0926\u0940\u0915\u0940 \u092A\u0942\u0930\u094D\u0923\u093E\u0902\
  \u0915 \u092F\u093E \u0928\u093F\u0930\u094D\u0926\u093F\u0937\u094D\u091F \u0938\
  \u091F\u0940\u0915\u0924\u093E \u0924\u0915 \u0938\u0902\u0936\u094B\u0927\u093F\
  \u0924 \u0915\u0930\u0928\u093E\u0964 \u0921\u0947\u0935\u0932\u092A\u0930\u094D\
  \u0938 \u0907\u0938\u0947 \u0938\u0930\u0932\u0940\u0915\u0930\u0923, \u0935\u093E\
  \u0938\u094D\u0924\u0935\u093F\u0915 \u0926\u0941\u0928\u093F\u092F\u093E \u0915\
  \u0940 \u0938\u0940\u092E\u093E\u0913\u0902 \u0915\u0947\u2026"
title: "\u0938\u0902\u0916\u094D\u092F\u093E\u0913\u0902 \u0915\u094B \u092A\u0942\
  \u0930\u094D\u0923\u093E\u0902\u0915 \u092C\u0928\u093E\u0928\u093E"
---

{{< edit_this_page >}}

## क्या और क्यों?
संख्याओं को गोल करने का मतलब है एक मूल्य को उसके नजदीकी पूर्णांक या निर्दिष्ट सटीकता तक संशोधित करना। डेवलपर्स इसे सरलीकरण, वास्तविक दुनिया की सीमाओं के अनुरूप या अतिरिक्त सटीकता को छोड़कर प्रदर्शन में सुधार के लिए करते हैं।

## कैसे:
C++ कई तरीके प्रदान करता है संख्याओं को गोल करने के लिए, जैसे कि `floor()`, `ceil()`, और `round()`:

```C++
#include <iostream>
#include <cmath> // गोल करने के फंक्शन्स के लिए

int main() {
    double num = 3.14;

    std::cout << "floor: " << std::floor(num) << "\n"; // आउटपुट: floor: 3
    std::cout << "ceil: " << std::ceil(num) << "\n";   // आउटपुट: ceil: 4
    std::cout << "round: " << std::round(num) << "\n"; // आउटपुट: round: 3

    // दो दशमलव तक गोल करने के लिए, जैसे कि :
    double precise_num = 3.146;
    double multiplier = 100.0;
    double rounded = std::round(precise_num * multiplier) / multiplier;

    std::cout << "दो दशमलव तक गोल की गई: " << rounded << "\n"; // आउटपुट: दो दशमलव तक गोल की गई: 3.15

    return 0;
}
```

## गहराई में
C++11 से पहले, गोल करने पर निर्भर था मैनुअल तकनीकों पर या गैर-मानक पुस्तकालयों पर। आज, `<cmath>` मजबूत तरीके प्रदान करता है। `floor()` नीचे की ओर गोल करता है, `ceil()` ऊपर की ओर गोल करता है, जबकि `round()` नजदीकी पूर्णांक की ओर जाता है, यहाँ तक कि 0.5 मामलों में टाई-ब्रेकिंग को भी संभालता है द्वारा सम संख्या में गोल करने के लिए।

इन फंक्शन्स के व्यवहार को समझना महत्वपूर्ण है; उदाहरण के लिए, नकारात्मक संख्याएँ आपको परेशान कर सकती हैं (`std::round(-2.5)` का परिणाम है `-2.0`).

विकल्प? सकारात्मक संख्याओँ के लिए 0.5 जोड़ने के बाद एक int में ढालना एक पारंपरिक हैक था लेकिन नकारात्मकों के साथ गलती करता है और यह प्रकार-अनभिज्ञाता नहीं है। बूस्ट जैसे पुस्तकालय अधिक सूक्ष्म दृष्टिकोण प्रदान कर सकते हैं, जबकि भाषा एक्सटेंशन या कंपाइलर इंट्रिंसिक्स विशिष्ट हार्डवेयर के लिए अनुकूलन कर सकते हैं।

## देखें भी
- C++ संदर्भ के लिए `<cmath>`: https://en.cppreference.com/w/cpp/header/cmath
- फ्लोटिंग-पॉइंट अंकगणित के लिए IEEE मानक (IEEE 754): https://ieeexplore.ieee.org/document/4610935
- बूस्ट न्यूमेरिक कन्वर्जन लाइब्रेरी: https://www.boost.org/doc/libs/release/libs/numeric/conversion/
