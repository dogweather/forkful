---
title:                "क्या एक निर्देशिका मौजूद है या नहीं देखें"
html_title:           "C++: क्या एक निर्देशिका मौजूद है या नहीं देखें"
simple_title:         "क्या एक निर्देशिका मौजूद है या नहीं देखें"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# क्यों

जब हम C++ में कोडिंग करते हैं, तो कई बार हमें की जानी चाहिए कि क्या दिए गए पथ पर कोई निर्देशिका मौजूद है या नहीं। इसके लिए हमें दिए गए पथ को सत्यापित करने की आवश्यकता होती है। यह जांच आपको अपने कोड को सुरक्षित और सही बनाने में मदद करती है।

## कैसे करें

```C++
#include <iostream>
#include <filesystem>

int main() {

    // दिए गए पथ को स्ट्रिंग की तरह घोषित करें
    std::string path = "C:\\Users\\User\\Documents\\Tutorial";

    // path_exists नामक फ़ंक्शन को बनाएं
    bool path_exists(const std::string& path) {
        // std::filesystem के उपयोग से दिए गए पथ का अस्तित्व जांचें
        return std::filesystem::exists(path);
    }

    // फ़ंक्शन को कॉल करें और पथ के अस्तित्व को दाखिल करें
    bool exists = path_exists(path);

    // If/else से अस्तित्व की जांच करें और समाचार संदेश प्रिंट करें
    if (exists) {
        std::cout << "दिए गए पथ में एक निर्देशिका मौजूद है।" << std::endl;
    }
    else {
        std::cout << "दिए गए पथ पर कोई निर्देशिका नहीं है।" << std::endl;
    }

    return 0;
}
```

आउटपुट:

```
दिए गए पथ में एक निर्देशिका मौजूद है।
```

## गहराई में जाएं

निर्देशिका का अस्तित्व जांचने के लिए, हम `std::filesystem` का उपयोग करते हैं जो C++17 से शुरू हुआ है। अगर आपका कोड C++17 से पहले के संस्करण पर आधारित है तो आप `filesystem` के बजाय `experimental/filesystem` का उपयोग कर सकते हैं। इस तरह से, आप निर्देशिका के अस्तित्व को सत्यापित कर सकते हैं और अपने कोड को सुरक्षित रख सकते हैं