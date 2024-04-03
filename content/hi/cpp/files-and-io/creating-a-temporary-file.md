---
date: 2024-01-20 17:40:45.493948-07:00
description: "\u0905\u0938\u094D\u0925\u093E\u092F\u0940 \u092B\u093C\u093E\u0907\u0932\
  \ \u092C\u0928\u093E\u0928\u093E \u092E\u0924\u0932\u092C \u0921\u0947\u091F\u093E\
  \ \u0915\u094B \u0905\u0938\u094D\u0925\u093E\u092F\u0940 \u0924\u094C\u0930 \u092A\
  \u0947 \u0938\u094D\u091F\u094B\u0930 \u0915\u0930\u0928\u093E \u091C\u093F\u0938\
  \u0947 \u092C\u093E\u0926 \u092E\u0947\u0902 \u0939\u091F\u093E\u092F\u093E \u091C\
  \u093E \u0938\u0915\u0947\u0964 \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\
  \u092E\u0930\u094D\u0938 \u0907\u0938\u0947 \u0907\u0938\u094D\u0924\u0947\u092E\
  \u093E\u0932 \u0915\u0930\u0924\u0947 \u0939\u0948\u0902 \u0915\u0948\u0936\u093F\
  \u0902\u0917, \u092C\u0921\u093C\u0940 \u092B\u093E\u0907\u0932 \u0915\u0947 \u0939\
  \u093F\u0938\u094D\u0938\u094B\u0902 \u0915\u094B\u2026"
lastmod: '2024-03-13T22:44:52.878751-06:00'
model: gpt-4-1106-preview
summary: "\u0905\u0938\u094D\u0925\u093E\u092F\u0940 \u092B\u093C\u093E\u0907\u0932\
  \ \u092C\u0928\u093E\u0928\u093E \u092E\u0924\u0932\u092C \u0921\u0947\u091F\u093E\
  \ \u0915\u094B \u0905\u0938\u094D\u0925\u093E\u092F\u0940 \u0924\u094C\u0930 \u092A\
  \u0947 \u0938\u094D\u091F\u094B\u0930 \u0915\u0930\u0928\u093E \u091C\u093F\u0938\
  \u0947 \u092C\u093E\u0926 \u092E\u0947\u0902 \u0939\u091F\u093E\u092F\u093E \u091C\
  \u093E \u0938\u0915\u0947\u0964 \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\
  \u092E\u0930\u094D\u0938 \u0907\u0938\u0947 \u0907\u0938\u094D\u0924\u0947\u092E\
  \u093E\u0932 \u0915\u0930\u0924\u0947 \u0939\u0948\u0902 \u0915\u0948\u0936\u093F\
  \u0902\u0917, \u092C\u0921\u093C\u0940 \u092B\u093E\u0907\u0932 \u0915\u0947 \u0939\
  \u093F\u0938\u094D\u0938\u094B\u0902 \u0915\u094B \u0938\u0902\u092D\u093E\u0932\
  \u0928\u0947 \u0914\u0930 \u0905\u0928\u094D\u092F \u091F\u0947\u092E\u094D\u092A\
  \u094B\u0930\u0930\u0940 \u0921\u0947\u091F\u093E-\u092A\u094D\u0930\u094B\u0938\
  \u0947\u0938\u093F\u0902\u0917 \u0915\u093E\u0930\u094D\u092F\u094B\u0902 \u0915\
  \u0947 \u0932\u093F\u090F\u0964."
title: "\u0905\u0938\u094D\u0925\u093E\u092F\u0940 \u092B\u093E\u0907\u0932 \u092C\
  \u0928\u093E\u0928\u093E"
weight: 21
---

## How to: (कैसे करें:)
```C++
#include <iostream>
#include <cstdio>
#include <fstream>

int main() {
    char temp_filename[] = "tempfileXXXXXX"; // XXXXXX को mkstemp यूनिक नाम में बदल देगा
    int file_descriptor = mkstemp(temp_filename); // अस्थायी फ़ाइल बनाएँ

    if(file_descriptor != -1) {
        std::cout << "Temporary file created: " << temp_filename << std::endl;

        // डेटा लिखने के लिए फ़ाइल स्ट्रीम का प्रयोग
        std::ofstream temp_file(temp_filename);
        temp_file << "Hello, World!";
        temp_file.close();

        // ... यहाँ कोड के अन्य भाग

        // फ़ाइल हटाना
        remove(temp_filename);
    } else {
        std::cerr << "Cannot create temporary file!" << std::endl;
    }

    return 0;
}
```

सैंपल आउटपुट:
```
Temporary file created: tempfilec1V069
```

## Deep Dive (गहराई से जानकारी):
हिस्टोरिकल पर्सपेक्टिव से, अस्थायी फ़ाइलों का इस्तेमाल लंबे समय से होता आ रहा है, खासकर जब ग्राहक-सर्वर आर्किटेक्चर और संबंधित तकनीकें पनप रही थीं। मॉडर्न सिस्टम में `tmpfile()` और `mkstemp()` जैसे सी स्टैंडर्ड लाइब्रेरी फंक्शनस् विश्वसनीयता और सुरक्षा में सुधार करते हैं। `mkstemp()` यूनिक फ़ाइल नाम बनाकर फ़ाइल के साथ एक फ़ाइल डिस्क्रिप्टर (file descriptor) लौटाता है, जबकि `tmpfile()` एक अनाम (anonymous) अस्थायी फ़ाइल बनाता है जो कार्यक्रम के समाप्त होते ही स्वतः हट जाती है।

विकल्पों में, ऑपरेटिंग सिस्टम्स उपलब्ध `/tmp` या `C:\Temp` जैसे अस्थायी फ़ोल्डरों में मैन्युअल रूप से अस्थायी फ़ाइलें बनाना शामिल है। एडवांस्ड यूज के लिए, लाइब्रेरीज़ जैसे कि Boost.FileSystem प्रोग्रामर्स को अधिकाधिक कंट्रोल और प्रदर्शन प्रदान करती हैं।

## See Also (यह भी देखें):
- C++ लाइब्रेरी डॉक्यूमेंटेशन: https://en.cppreference.com/w/
- Boost FileSystem लाइब्रेरी: https://www.boost.org/doc/libs/1_75_0/libs/filesystem/doc/index.htm
- सिक्योर टेम्प फ़ाइल्स (Secure Temp Files) के बारे में CERT गाइड: https://www.securecoding.cert.org/confluence/display/cs/Creating+Temporary+Files+Securely
