---
title:                "डायरेक्टरी मौजूद है या नहीं, जांचना"
html_title:           "C++: डायरेक्टरी मौजूद है या नहीं, जांचना"
simple_title:         "डायरेक्टरी मौजूद है या नहीं, जांचना"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# आपका स्वागत है! 

## क्या और क्यों?

- "डिरेक्टरी मौजूद है या नहीं", उनके अस्तित्व की जाँच करना कहलाता है। 
- इसे सुनिश्‍चित करने के लिए कि आपकी प्रोग्राम के द्वारा किसी विशेष डिरेक्टरी से डेटा आ रहा है या नहीं।

## कैसे:

कोड उदाहरण और उदाहरण आउटपुट निम्नानुसार होगा:

```Cpp
#include <filesystem>
#include <iostream>

int main() {
    if (std::filesystem::exists("directory_path")) {
        std::cout << "Directory exists\n";
    }
    else {
        std::cout << "Directory does not exist\n";
    }
    return 0;
}
```
यदि डिरेक्टरी मौजूद हो, तो `"Directory exists"` प्रिंट होगा, अन्यथा `"Directory does not exist"` प्रिंट होगा।


## गहरी डाइविंग:

- C++17 के साथ `std::filesystem` की कक्षा मौजूद है। यह आधिकारिक रूप से डिरेक्टरी की जांच करने का संदर्भ है।
- `boost::filesystem` एक वैकल्पिक उपयोग है जो अधिक पुराने संस्करणों में काम करता है।
- `std::filesystem::exists()` वास्तविक रूप से एक `error_code` तत्व वर्तमान होने पर वापस आता है जो डिरेक्टरी मूल्यांकन के समय मौजूद है या नहीं।

## देखने के लिए यह भी:

- [C++ std::filesystem](https://en.cppreference.com/w/cpp/filesystem)
- [Boost filesystem](https://www.boost.org/doc/libs/1_75_0/libs/filesystem/doc/index.htm)
- [Filesystem Design with POSIX Interface](https://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap04.html) 

यहां आपको C++ में बुनियादी डिरेक्टरी प्रेक्षापण का परिचय मिलेगा। आपकी जानकारी और उन्नति के लिए, ऊपर लिंक प्रदान की गई हैं।