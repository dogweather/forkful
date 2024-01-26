---
title:                "यह जांचना कि डायरेक्टरी मौजूद है या नहीं"
html_title:           "Arduino: यह जांचना कि डायरेक्टरी मौजूद है या नहीं"
simple_title:         "यह जांचना कि डायरेक्टरी मौजूद है या नहीं"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)

डायरेक्टरी की जाँच से मतलब है कि क्या एक निश्चित पाथ पर फोल्डर मौजूद है या नहीं का पता लगाना। प्रोग्रामर इसे इसलिए करते हैं ताकि वे सुनिश्चित कर सकें कि डेटा को सहेजने, खोलने या मोडिफाई करने से पहले वह डायरेक्टरी मौजूद हो।

## How to: (कैसे करें:)

```c++
#include <iostream>
#include <filesystem>

int main() {
    std::string directory_path = "/path/to/your/directory";

    // std::filesystem का उपयोग कर के डायरेक्टरी की जाँच करें
    if (std::filesystem::exists(directory_path)) {
        std::cout << "Directory exists: " << directory_path << std::endl;
    } else {
        std::cout << "Directory does not exist: " << directory_path << std::endl;
    }

    return 0;
}
```

सैंपल आउटपुट:

```
Directory exists: /path/to/your/directory
```
या
```
Directory does not exist: /path/to/your/directory
```

## Deep Dive (गहराई से जानकारी):

पहले, डायरेक्टरी की जाँच POSIX सिस्टम कॉल `stat` का उपयोग करके की जाती थी। लेकिन C++17 के बाद से, `std::filesystem` मॉड्यूल पेश किया गया जो फाइल सिस्टम ऑपरेशन को मानकीकृत करता है। इससे पोर्टेबल कोड लिखना आसान हो जाता है।

वैकल्पिक तरीके में बूस्ट फाइलसिस्टम लाइब्रेरी या `dirent.h` (POSIX स्पेसिफिक) शामिल हैं, पर `std::filesystem` का उपयोग करना ज्यादा सीधा और मानकीकृत है।

डायरेक्टरी की जाँच का व्यवहार प्लेटफार्म पर निर्भर करता है, और यह व्यवहार एक्सेस परमीशन, फाइल सिस्टम की सुरक्षा सेटिंग्स, और सिम्लिंक्स के साथ विवर्तनिक हो सकता है। इसलिए, अच्छे प्रोग्राम को इन सब विचारों को महत्व देना चाहिए।

## See Also (देखें भी):

- [Filesystem library documentation](https://en.cppreference.com/w/cpp/filesystem)
- [Boost.Filesystem Tutorial](https://www.boost.org/doc/libs/1_75_0/libs/filesystem/doc/tutorial.html)
- [ISO C++ Standards Committee - Filesystem Technical Specification](https://isocpp.org/std/the-standard)
