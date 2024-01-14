---
title:                "C++: वर्तमान तारीख प्राप्त करना"
programming_language: "C++"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## क्यों

क्या आपको कभी अपने प्रोग्राम में वर्तमान दिनांक को प्राप्त करने की जरूरत हुई है? छिपी हुई व्यावसायिक लक्ष्यों हो या सादा समय-प्रबंधन के लिए, हर किसी को वर्तमान दिनांक को प्राप्त करना महत्वपूर्ण हो सकता है। इस ब्लॉग पोस्ट के माध्यम से, मैं आपको सी++ में वर्तमान दिनांक को प्राप्त करने के लिए कुछ आसान तरीके बताऊंगा।

## कैसे करें

अपने सी++ कोड में वर्तमान दिनांक को प्राप्त करने के लिए, हम `ctime` और `chrono` हेडर फ़ाइल्स का उपयोग कर सकते हैं। नीचे एक उदाहरण दिया गया है जो आपको समझने में मदद करेगा:

```C++
#include <iostream>
#include <ctime>
#include <chrono>

int main()
{
    // प्राप्त करें वर्तमान समय का विश्लेषण
    time_t now = time(0);

    // अपने वर्तमान समय की स्ट्रक्टर क्रियेट करें
    tm *currentTime = localtime(&now);

    // समय को फॉर्मेट करें और प्रिंट करें
    std::cout << "वर्तमान दिनांक: " << (currentTime->tm_mon + 1) << "/" << (currentTime->tm_mday) << "/" << (currentTime->tm_year + 1900) << std::endl;

    // वर्तमान समय को मिलिसेकंड में प्राप्त करें
    auto milliseconds_since_epoch = std::chrono::system_clock::now().time_since_epoch() / std::chrono::milliseconds(1);
    std::cout << "वर्तमान समय की मिलिसेकंड: " << milliseconds_since_epoch << std::endl;

    return 0;
}
```

आउटपुट:

वर्तमान दिनांक: 10/22/2019
वर्तमान समय की मिलिसेकंड: 1571717289906

## गहराई में

आपने ऊपर दिए गए कोड में देखा होगा कि हमने `time` और `system_clock` के साथ काम किया है। `time` दिनांक और समय को प्राप्त करने के लिए उपयोग किया जाता है जबकि `system_clock