---
title:                "TOML के साथ काम करना"
date:                  2024-01-26T04:20:41.952201-07:00
model:                 gpt-4-0125-preview
simple_title:         "TOML के साथ काम करना"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/working-with-toml.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
TOML (टॉम की स्पष्ट, न्यूनतम भाषा) एक डाटा सीरियलाइज़ेशन फॉर्मेट है जिसे इसके स्पष्ट सेमेंटिक्स के कारण पढ़ना आसान होता है। प्रोग्रामर्स TOML का उपयोग कॉन्फ़िगरेशन फाइलों के लिए करते हैं क्योंकि यह मानव पठनीयता और मशीन पार्सबिलिटी के बीच एक संतुलन बनाता है।

## कैसे:
C++ में TOML के साथ काम करने के लिए, आपको `toml++` जैसी लाइब्रेरी की आवश्यकता होगी। यहाँ एक त्वरित शुरुआत है:

```C++
#include <toml++/toml.h>
#include <iostream>
#include <fstream>

int main() {
    // एक फाइल से TOML पार्स करें
    std::ifstream ifs("config.toml");
    auto config = toml::parse(ifs);

    // एक मान तक पहुँचना
    std::string title = config["title"].value_or("Untitled");
    std::cout << "Title: " << title << '\n';

    // TOML को संशोधित करें और सेव करें
    config["title"] = "New Title";
    std::ofstream ofs("config.toml");
    ofs << config;
}
```

`config.toml` नमूना:
```toml
title = "Example"
```

नमूना आउटपुट:
```plaintext
Title: Example
```

## गहराई में
TOML को 2013 में टॉम प्रेस्टन-वर्नर द्वारा YAML और JSON के विकल्प के रूप में बनाया गया था। यह विशेष रूप से कॉन्फ़िगरेशन फाइलों के लिए सरल और स्पष्ट होने के लिए डिज़ाइन किया गया है। JSON के विपरीत, TOML अस्पष्टता पर ध्यान केंद्रित करता है, जिसका अर्थ है यह दस्तावेज़ को पार्स करने में निर्णायक है।

TOML के विकल्पों में YAML शामिल है, जो अनुमति में अधिक उदार है, हालांकि कभी-कभी अनिश्चितता की लागत पर। एक और विकल्प JSON है, जो संरचना में काफी सख्त है लेकिन कॉन्फ़िगरेशन के लिए इसकी कमी, टिप्पणियों की कमी, और इसकी ब्रेस-भारी सिंटैक्स के कारण मानव-अनुकूल नहीं है।

कार्यान्वयन में, `toml++` एक हेडर-केवल C++17 लाइब्रेरी है जो नवीनतम TOML स्पेसिफिकेशन के अनुरूप है। यह TOML डेटा को नेविगेट और मैनिपुलेट करने के लिए एक DOM-जैसे इंटरफेस प्रदान करती है, जिसे परियोजनाओं में एकीकृत करने के लिए सीधा बनाया गया है। लाइब्रेरी पार्सिंग, वैलिडेशन, और आउटपुट जनरेशन का ध्यान रखती है, जिससे आप C++ प्रकारों का उपयोग करके TOML डेटा प्राप्त कर सकते हैं और सेट कर सकते हैं।

## और देखें
- TOML GitHub भंडार: https://github.com/toml-lang/toml
- `toml++`, TOML के लिए एक C++ लाइब्रेरी: https://github.com/marzer/tomlplusplus
- फॉर्मेट के विस्तृत विवरणों के साथ आधिकारिक TOML दस्तावेज़ीकरण: https://toml.io/en/