---
title:                "एक अस्थायी फ़ाइल बनाना"
html_title:           "C++: एक अस्थायी फ़ाइल बनाना"
simple_title:         "एक अस्थायी फ़ाइल बनाना"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Temporary File बनाने का तरीका

## क्या है और क्यों?

C++ में एक **temporary file** बनाना एक ऐसी क्रिया होती है जिसके द्वारा कोड चलाने के दौरान आस्थायिक डेटा संग्रहीत किया जा सकता है। इसे उपयोग करने की मुख्य वजह है डेटा को आवश्यकतानुसार तत्परता से सहेजना और पुन: प्राप्त करना।

## कैसे करें: 

C++ की `fstream` library का उपयोग करके हम एक temporary file बना सकते हैं। आइए देखें:

``` C++
#include <fstream>
#include <iostream>

int main() {
    std::fstream temp_file;

    // Temporary file बना रहे हैं
    temp_file.open("temp.txt", std::ios::out);

    // डेटा लिख रहे हैं
    temp_file << "Hello, World!";

    // File close कर रहे हैं
    temp_file.close();
    return 0;
}
```
जब यह कोड चलता है, तो यह एक `temp.txt` नामक temporary file बनाएगा और `Hello, World!` के साथ इसमें डेटा लिखेगा।

## Deep Dive

Temporary files को पहली बार Unix operating system में इंट्रोड्यूस किया गया था। इसका उपयोग मुख्य रूप से डेटा को क्राश, पावर लॉस या अन्य प्रॉब्लेम से बचाने के लिए किया जाता है।

Temporary files के alternatives के रूप में, आप memory-based data structures जैसे कि vectors और lists का उपयोग कर सकते हैं। हालांकि, वे बड़ी फाइलों और डेटा के साथ काम करने में कम असरदार होते हैं।

## See Also

कृपया Unit और C++ reference में और अधिक जानकारी के लिए देखें:
- [C++ File I/O](https://www.w3schools.com/cpp/cpp_files.asp)
- [C++ Standard Library: Input/Output with Files](http://www.cplusplus.com/doc/tutorial/files/)