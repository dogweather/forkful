---
title:                "अस्थायी फाइल बनाना"
aliases:
- hi/cpp/creating-a-temporary-file.md
date:                  2024-01-20T17:40:45.493948-07:00
model:                 gpt-4-1106-preview
simple_title:         "अस्थायी फाइल बनाना"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)

अस्थायी फ़ाइल बनाना मतलब डेटा को अस्थायी तौर पे स्टोर करना जिसे बाद में हटाया जा सके। प्रोग्रामर्स इसे इस्तेमाल करते हैं कैशिंग, बड़ी फाइल के हिस्सों को संभालने और अन्य टेम्पोररी डेटा-प्रोसेसिंग कार्यों के लिए।

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
