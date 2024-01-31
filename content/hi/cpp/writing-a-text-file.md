---
title:                "टेक्स्ट फाइल लिखना"
date:                  2024-01-19
html_title:           "Bash: टेक्स्ट फाइल लिखना"
simple_title:         "टेक्स्ट फाइल लिखना"

category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/writing-a-text-file.md"
---

{{< edit_this_page >}}

## क्या और क्यों? (What & Why?)
C++ में टेक्स्ट फ़ाइल लिखने का मतलब है कि डेटा को फ़ाइल में सहेजना। यह कार्यक्रम के डेटा को दीर्घकालिक संचय के लिए और ऑफ़लाइन एनालिसिस की सुविधा के लिए किया जाता है।

## कैसे करें? (How to:)
```C++
#include <fstream>
#include <iostream>

int main() {
    std::ofstream outfile("example.txt");
    
    if (outfile.is_open()) {
        outfile << "नमस्ते, यह मेरी पहली टेक्स्ट फ़ाइल है!\n";
        outfile << "C++ से फ़ाइल लिखना आसान है।";
        outfile.close();
    } else {
        std::cerr << "फ़ाइल खोलने में असमर्थ!\n";
    }

    return 0;
}
```
आउटपुट: `example.txt` फाइल में दो लाइनें होंगी:
```
नमस्ते, यह मेरी पहली टेक्स्ट फ़ाइल है!
C++ से फ़ाइल लिखना आसान है।
```

## गहराई से जानकारी (Deep Dive)
पारंपरिक रूप से, C++ फ़ाइल हैंडलिंग `iostream` लाइब्रेरी का उपयोग करता है। इसमें `<fstream>` हेडर फ़ाइल `ofstream` (आउटपुट फ़ाइल स्ट्रीम) क्लास को प्रदान करता है। विकल्प के रूप में low-level C functions जैसे `fprintf` और `fwrite` होते हैं, लेकिन C++ स्ट्रीम अधिक सुरक्षित और आसानी से उपयोग करने योग्य हैं। फ़ाइल सिस्टम, एनकोडिंग, और एक्सेस परमिशन जैसे पहलुओं पर चर्चा, फ़ाइल राइटिंग की गहराई को दर्शाते हैं।

## सम्बंधित स्रोत देखें (See Also)
- C++ रेफरेंस फॉर फाइल I/O: http://www.cplusplus.com/reference/fstream/
- C++ ट्यूटोरियल फॉर फ़ाइल I/O: https://www.learncpp.com/cpp-tutorial/file-io/
- एडवांस्ड फ़ाइल ऑपरेशंस: https://en.cppreference.com/w/cpp/io/basic_fstream
