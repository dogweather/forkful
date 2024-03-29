---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:07:43.458291-07:00
description: "\u090F\u0915 \u0928\u093F\u0930\u094D\u0926\u0947\u0936\u093F\u0915\u093E\
  \ \u0915\u0947 \u0905\u0938\u094D\u0924\u093F\u0924\u094D\u0935 \u0915\u0940 \u091C\
  \u093E\u0901\u091A \u0915\u0930\u0928\u093E, \u0907\u0938\u0915\u093E \u092E\u0924\
  \u0932\u092C \u0939\u0948 \u0915\u093F\u0938\u0940 \u0928\u093F\u0936\u094D\u091A\
  \u093F\u0924 \u092A\u0925 \u092A\u0930 \u090F\u0915 \u0928\u093F\u0930\u094D\u0926\
  \u0947\u0936\u093F\u0915\u093E \u0915\u0947 \u092E\u094C\u091C\u0942\u0926 \u0939\
  \u094B\u0928\u0947 \u0915\u093E \u0928\u093F\u0930\u094D\u0927\u093E\u0930\u0923\
  \ \u0915\u0930\u0928\u093E, \u0909\u0938\u0915\u0947 \u092D\u0940\u0924\u0930 \u0938\
  \u0947 \u092B\u093C\u093E\u0907\u0932\u094B\u0902 \u0915\u094B \u092A\u0922\u093C\
  \u0928\u0947 \u092F\u093E \u0909\u0928\u2026"
lastmod: '2024-03-13T22:44:52.869975-06:00'
model: gpt-4-0125-preview
summary: "\u090F\u0915 \u0928\u093F\u0930\u094D\u0926\u0947\u0936\u093F\u0915\u093E\
  \ \u0915\u0947 \u0905\u0938\u094D\u0924\u093F\u0924\u094D\u0935 \u0915\u0940 \u091C\
  \u093E\u0901\u091A \u0915\u0930\u0928\u093E, \u0907\u0938\u0915\u093E \u092E\u0924\
  \u0932\u092C \u0939\u0948 \u0915\u093F\u0938\u0940 \u0928\u093F\u0936\u094D\u091A\
  \u093F\u0924 \u092A\u0925 \u092A\u0930 \u090F\u0915 \u0928\u093F\u0930\u094D\u0926\
  \u0947\u0936\u093F\u0915\u093E \u0915\u0947 \u092E\u094C\u091C\u0942\u0926 \u0939\
  \u094B\u0928\u0947 \u0915\u093E \u0928\u093F\u0930\u094D\u0927\u093E\u0930\u0923\
  \ \u0915\u0930\u0928\u093E, \u0909\u0938\u0915\u0947 \u092D\u0940\u0924\u0930 \u0938\
  \u0947 \u092B\u093C\u093E\u0907\u0932\u094B\u0902 \u0915\u094B \u092A\u0922\u093C\
  \u0928\u0947 \u092F\u093E \u0909\u0928\u2026"
title: "\u0921\u093E\u092F\u0930\u0947\u0915\u094D\u091F\u0930\u0940 \u092E\u094C\u091C\
  \u0942\u0926 \u0939\u0948 \u092F\u093E \u0928\u0939\u0940\u0902, \u0915\u0948\u0938\
  \u0947 \u091C\u093E\u0902\u091A\u0947\u0902"
---

{{< edit_this_page >}}

## क्या और क्यों?
एक निर्देशिका के अस्तित्व की जाँच करना, इसका मतलब है किसी निश्चित पथ पर एक निर्देशिका के मौजूद होने का निर्धारण करना, उसके भीतर से फ़ाइलों को पढ़ने या उन पर लिखने जैसे क्रियाकलापों को करने से पूर्व। प्रोग्रामर इसे फ़ाइल संचालनों से संबंधित त्रुटियों से बचने के लिए करते हैं, इससे उनके अनुप्रयोगों में फ़ाइल कार्यों का संचालन अधिक सहज और विश्वसनीय बनता है।

## कैसे करें:
आधुनिक C++ (C++17 और उसके बाद) में, आप फ़ाइलसिस्टम लाइब्रेरी का उपयोग करके जाँच सकते हैं कि कोई निर्देशिका मौजूद है या नहीं। यह फ़ाइलसिस्टम संचालन, जिनमें निर्देशिका के अस्तित्व की जाँच शामिल है, को करने के लिए एक सरल और मानकीकृत तरीका प्रदान करता है।

```cpp
#include <iostream>
#include <filesystem>

namespace fs = std::filesystem;

int main() {
    const fs::path dirPath = "/path/to/directory";

    if (fs::exists(dirPath) && fs::is_directory(dirPath)) {
        std::cout << "निर्देशिका मौजूद है।" << std::endl;
    } else {
        std::cout << "निर्देशिका मौजूद नहीं है।" << std::endl;
    }

    return 0;
}
```
अगर निर्देशिका मौजूद है तो सैंपल आउटपुट:
```
निर्देशिका मौजूद है।
```

अगर निर्देशिका मौजूद नहीं है तो सैंपल आउटपुट:
```
निर्देशिका मौजूद नहीं है।
```

जिन परियोजनाओं में अभी तक C++17 का उपयोग नहीं किया जा रहा है या अतिरिक्त विशेषताओं के लिए, बूस्ट फ़ाइलसिस्टम लाइब्रेरी एक लोकप्रिय तृतीय-पक्ष विकल्प है जो समान कार्यक्षमता प्रदान करती है।

```cpp
#include <iostream>
#include <boost/filesystem.hpp>

namespace fs = boost::filesystem;

int main() {
    const fs::path dirPath = "/path/to/directory";

    if (fs::exists(dirPath) && fs::is_directory(dirPath)) {
        std::cout << "निर्देशिका मौजूद है।" << std::endl;
    } else {
        std::cout << "निर्देशिका मौजूद नहीं है।" << std::endl;
    }

    return 0;
}
```
बूस्ट फ़ाइलसिस्टम का उपयोग करते हुए, आउटपुट C++17 फ़ाइलसिस्टम उदाहरण के समान होगा, निर्दिष्ट पथ पर निर्देशिका की मौजूदगी पर निर्भर करता है।
