---
title:                "डायरेक्टरी मौजूद है या नहीं, कैसे जांचें"
aliases:
- /hi/cpp/checking-if-a-directory-exists.md
date:                  2024-02-03T19:07:43.458291-07:00
model:                 gpt-4-0125-preview
simple_title:         "डायरेक्टरी मौजूद है या नहीं, कैसे जांचें"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
